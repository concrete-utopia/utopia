import * as Matter from 'matter-js'
import Utils from '../../utils/utils'
import { LocalPoint, LocalVector } from '../../core/shared/math-utils'

export type UtopiaSpring = {
  tension: number
  friction: number
  bodyA: Matter.Body | null
  bodyB: Matter.Body | null
  pointA: LocalPoint
  pointB: LocalPoint
  length: number
  type: string
}

type SpringOptions = {
  tension: number
  friction: number
  bodyA?: Matter.Body
  bodyB?: Matter.Body
  pointA?: LocalPoint
  pointB?: LocalPoint
  length?: number
}

type SpringDerivative = {
  dx: LocalVector
  dv: LocalVector
}

type RK4SpringState = {
  delta: LocalVector
  velocity: LocalVector
  tension: number
  friction: number
}

export type RK4SpringStep = {
  delta: LocalVector
  velocity: LocalVector
  force: LocalVector
}

function calcSpringAcceleration(state: RK4SpringState): LocalVector {
  const { delta, tension, friction, velocity } = state
  return {
    x: tension * delta.x - friction * velocity.x,
    y: tension * delta.y - friction * velocity.y,
  } as LocalVector
}

function evaluateState(state: RK4SpringState): SpringDerivative {
  return {
    dx: Utils.negate(state.velocity),
    dv: calcSpringAcceleration(state),
  }
}

function evaluateStateUsingDerivatives(
  state: RK4SpringState,
  dt: number,
  dxdv: SpringDerivative,
): SpringDerivative {
  const newState: RK4SpringState = {
    delta: Utils.addVectors(state.delta, Utils.scaleVector(dxdv.dx, dt)),
    velocity: Utils.addVectors(state.velocity, Utils.scaleVector(dxdv.dv, dt)),
    tension: state.tension,
    friction: state.friction,
  }

  return evaluateState(newState)
}

function springIntegrateState(
  state: RK4SpringState,
  timePassed: number,
): { delta: LocalVector; velocity: LocalVector; dvdt: LocalVector } {
  const a = evaluateState(state)
  const b = evaluateStateUsingDerivatives(state, timePassed * 0.5, a)
  const c = evaluateStateUsingDerivatives(state, timePassed * 0.5, b)
  const d = evaluateStateUsingDerivatives(state, timePassed, c)

  function add(d1: SpringDerivative, d2: SpringDerivative) {
    return {
      dx: Utils.addVectors(d1.dx, d2.dx),
      dv: Utils.addVectors(d1.dv, d2.dv),
    }
  }

  function mult(derivative: SpringDerivative, n: number) {
    return {
      dx: Utils.scaleVector(derivative.dx, n),
      dv: Utils.scaleVector(derivative.dv, n),
    }
  }

  // dxdt = 1.0/6.0 * (a.dx + 2.0 * (b.dx + c.dx) + d.dx)
  // dvdt = 1.0/6.0 * (a.dv + 2.0 * (b.dv + c.dv) + d.dv)
  const res = mult(mult(add(a, add(mult(add(b, c), 2), d)), 1.0 / 6.0), timePassed)

  return {
    delta: Utils.addVectors(state.delta, res.dx),
    velocity: Utils.addVectors(state.velocity, res.dv),
    dvdt: res.dv,
  }
}

function updateSprings(springs: Array<UtopiaSpring>, timePassed: number) {
  for (var spring of springs) {
    const { bodyA, bodyB, pointA, pointB, tension, friction, length } = spring

    const p1 =
      bodyA != null
        ? Utils.offsetPoint(bodyA.position as LocalPoint, Utils.rotateVector(pointA, bodyA.angle))
        : pointA
    const p2 =
      bodyB != null
        ? Utils.offsetPoint(bodyB.position as LocalPoint, Utils.rotateVector(pointB, bodyB.angle))
        : pointB

    const v1 = (bodyA != null ? bodyA.velocity : Utils.zeroPoint) as LocalVector
    const v2 = (bodyB != null ? bodyB.velocity : Utils.zeroPoint) as LocalVector
    const v = Utils.addVectors(v1, v2)
    const scaledV = Utils.scaleVector(v, 1000 / timePassed)

    const deltaBefore = {
      x: p2.x - p1.x,
      y: p2.y - p1.y,
    }
    const { delta } = UtopiaSprings.stepSpring(
      timePassed / 1000,
      p1,
      p2,
      scaledV,
      tension,
      friction,
      length,
    )
    const velocity = Utils.addVectors(deltaBefore as LocalVector, Utils.negate(delta))
    const vDelta = Utils.addVectors(velocity, Utils.negate(v))
    const acceleration = Utils.scaleVector(vDelta, 1 / timePassed)
    const force = acceleration

    if (bodyA != null) {
      Matter.Body.applyForce(bodyA, p1, force)
    }

    if (bodyB != null) {
      Matter.Body.applyForce(bodyB, p2, Utils.negate(force))
    }
  }
}

export const UtopiaSprings = {
  // FIXME Right now this _almost_ works, but when multiple springs are attached this completely blows up
  // TODO figure out the types

  name: 'utopia-springs',
  version: '0.0.1',
  for: 'matter-js@^0.12.0',

  // installs the plugin where `base` is `Matter`
  // you should not need to call this directly.
  install: function (base: any) {
    base.after('Engine.create', function () {
      // @ts-ignore
      UtopiaSprings.Engine.init(this)
    })

    base.before('Engine.update', function (engine: any, timePassed: number) {
      UtopiaSprings.Engine.beforeUpdate(engine, timePassed)
    })

    // TODO Handle correctly adding to the world and to composites via World.add(world, spring)
  },

  Engine: {
    init: function (engine: any) {
      engine.world.plugin.utopiaSprings = engine.world.plugin.utopiaSprings || []
    },

    beforeUpdate: function (engine: any, timePassed: number) {
      const world = engine.world
      updateSprings(world.plugin.utopiaSprings, timePassed)
    },
  },

  create: function (options: SpringOptions): UtopiaSpring {
    return {
      bodyA: options.bodyA || null,
      bodyB: options.bodyB || null,
      pointA: options.pointA || (Utils.zeroPoint as LocalPoint),
      pointB: options.pointB || (Utils.zeroPoint as LocalPoint),
      tension: options.tension,
      friction: options.friction,
      length: options.length || 0,
      type: 'utopiaSpring',
    }
  },

  stepSpring: function (
    timePassed: number,
    p1: LocalPoint,
    p2: LocalPoint,
    v: LocalVector,
    tension: number,
    friction: number,
    springLength: number = 0,
  ): RK4SpringStep {
    const stateBefore = {
      // TODO normalise spring length and factor that into the delta
      delta: {
        x: p2.x - p1.x,
        y: p2.y - p1.y,
      } as LocalVector,
      velocity: v,
      tension: tension,
      friction: friction,
    }

    const { delta, velocity, dvdt } = springIntegrateState(stateBefore, timePassed)

    return {
      delta: delta,
      velocity: velocity,
      force: dvdt,
    }
  },
}
