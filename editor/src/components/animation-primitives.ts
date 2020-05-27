import * as linearEase from 'eases/linear'
import * as quadInEase from 'eases/quad-in'
import * as quadInOutEase from 'eases/quad-in-out'
import * as quadOutEase from 'eases/quad-out'
import { LocalPoint } from '../core/shared/math-utils'
import { bouncinessSpeedToTensionFriction } from './physics/spring-converters'
import { UtopiaSprings } from './physics/springs'

export type EasingFunction = any

export interface Spring2DResult {
  newVelocity: LocalPoint
  newValue: LocalPoint
}

export type SpringResult = {
  newVelocity: number
  newValue: number
}

export type Repeat = number | boolean

export function applyEasing(
  easingFunction: EasingFunction,
  startValue: number,
  targetValue: number | null,
  startTime: number,
  duration: number,
  currentTime: number,
): number {
  const currentTimeAfterStart = currentTime - startTime
  const timeFractionPassed = currentTimeAfterStart / duration
  if (timeFractionPassed < 0) {
    return startValue
  } else if (timeFractionPassed > 1) {
    if (targetValue === null || targetValue === undefined) {
      // Not really a fan, but fingers crossed this will never occur.
      console.error(`Invalid undefined target value for non-function based easing.`)
      return startValue
    } else {
      return targetValue
    }
  } else {
    if (targetValue === null || targetValue === undefined) {
      // Not really a fan, but fingers crossed this will never occur.
      console.error(`Invalid undefined target value for non-function based easing.`)
      return startValue
    } else {
      let ease: (time: number) => number
      if (typeof easingFunction === 'function') {
        ease = easingFunction
      } else {
        switch (easingFunction) {
          case 'linear':
            ease = linearEase
            break
          case 'easein':
            ease = quadInEase
            break
          case 'easeout':
            ease = quadOutEase
            break
          case 'easeinout':
            ease = quadInOutEase
            break
          default:
            console.error(
              `Unexpected easing function passed (${easingFunction}), defaulting to linear.`,
            )
            ease = linearEase
        }
      }
      const easedTimeFraction = ease(timeFractionPassed)
      const animationDiff = targetValue - startValue
      return startValue + animationDiff * easedTimeFraction
    }
  }
}

export function calculate2DSpring(
  bounciness: number,
  speed: number,
  currentVelocity: LocalPoint,
  currentValue: LocalPoint,
  targetValue: LocalPoint,
  timeOfLastRun: number,
  timeNow: number,
): Spring2DResult {
  // HACK clamping seconds passed to avoid extreme velocity
  const timePassed = Math.max(Math.min(timeNow - timeOfLastRun, 70), 16)
  const p1 = currentValue as LocalPoint
  const v1 = currentVelocity as LocalPoint
  const p2 = targetValue as LocalPoint
  const { tension, friction } = bouncinessSpeedToTensionFriction(bounciness, speed)
  const { delta, velocity } = UtopiaSprings.stepSpring(
    timePassed / 1000,
    p1,
    p2,
    v1,
    tension,
    friction,
  )
  const newValue = {
    x: targetValue.x - delta.x,
    y: targetValue.y - delta.y,
  } as LocalPoint
  return {
    newVelocity: velocity,
    newValue: newValue,
  }
}

export function calculateBouncySpring(
  bounciness: number,
  speed: number,
  currentVelocity: number,
  currentValue: number,
  targetValue: number,
  timeOfLastRun: number,
  timeNow: number,
): SpringResult {
  const currentVelocityNumber = currentVelocity || 0
  const currentVelocity2D = { x: currentVelocityNumber, y: 0 } as LocalPoint
  const currentValue2D = { x: currentValue, y: 0 } as LocalPoint
  const targetValue2D = { x: targetValue, y: 0 } as LocalPoint
  const springResult = calculate2DSpring(
    bounciness,
    speed,
    currentVelocity2D,
    currentValue2D,
    targetValue2D,
    timeOfLastRun,
    timeNow,
  )

  return {
    newVelocity: springResult.newVelocity.x,
    newValue: springResult.newValue.x,
  }
}
