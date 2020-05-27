type TensionFriction = {
  tension: number
  friction: number
}

/*
 * The below code comes from a slight rewrite of
 * https://github.com/facebook/rebound-js/blob/master/src/BouncyConversion.js
 * Which has the following license (copied and pasted from https://github.com/facebook/rebound-js/blob/master/LICENSE):
 *
 * BSD License
 *
 * For the rebound-js software
 *
 * Copyright (c) 2014, Facebook, Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name Facebook nor the names of its contributors may be used to
 *    endorse or promote products derived from this software without specific
 *    prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

export function bouncinessSpeedToTensionFriction(
  bounciness: number,
  speed: number,
): TensionFriction {
  const normalisedBounciness = bounciness / 1.7 / 20
  const normalisedSpeed = speed / 1.7 / 20
  const projectedBounciness = normalisedBounciness * 0.8
  const projectedSpeed = 0.5 + normalisedSpeed * 199.5

  const tension = projectedSpeed

  return {
    tension: tension,
    friction: quadraticOutInterpolation(projectedBounciness, b3Nobounce(tension), 0.01),
  }
}

function quadraticOutInterpolation(t: number, start: number, end: number): number {
  return linearInterpolation(2 * t - t * t, start, end)
}

function linearInterpolation(t: number, start: number, end: number) {
  return t * end + (1.0 - t) * start
}

function b3Friction1(x: number): number {
  return 0.0007 * Math.pow(x, 3) - 0.031 * Math.pow(x, 2) + 0.64 * x + 1.28
}

function b3Friction2(x: number): number {
  return 0.000044 * Math.pow(x, 3) - 0.006 * Math.pow(x, 2) + 0.36 * x + 2
}

function b3Friction3(x: number): number {
  return 0.00000045 * Math.pow(x, 3) - 0.000332 * Math.pow(x, 2) + 0.1078 * x + 5.84
}

function b3Nobounce(tension: number): number {
  if (tension <= 18) {
    return b3Friction1(tension)
  } else if (tension > 18 && tension <= 44) {
    return b3Friction2(tension)
  } else {
    return b3Friction3(tension)
  }
}
