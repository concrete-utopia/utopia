/*
 *          __        ___
 *    _____/ /___  __/ (_)____
 *   / ___/ __/ / / / / / ___/
 *  (__  ) /_/ /_/ / / (__  )
 * /____/\__/\__, /_/_/____/
 *          /____/
 *
 * light - weight css preprocessor @licence MIT
 */
/* eslint-disable */
(function (factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? (module['exports'] = factory()) :
		typeof define === 'function' && define['amd'] ? define(factory()) :
			(window['stylis'] = factory())
}(function () {

	'use strict'

	/**
	 * Notes
	 *
	 * The ['<method name>'] pattern is used to support closure compiler
	 * the jsdoc signatures are also used to the same effect
	 *
	 * ---- 
	 *
	 * int + int + int === n4 [faster]
	 *
	 * vs
	 *
	 * int === n1 && int === n2 && int === n3
	 *
	 * ----
	 *
	 * switch (int) { case ints...} [faster]
	 *
	 * vs
	 *
	 * if (int == 1 && int === 2 ...)
	 *
	 * ----
	 *
	 * The (first*n1 + second*n2 + third*n3) format used in the property parser
	 * is a simple way to hash the sequence of characters
	 * taking into account the index they occur in
	 * since any number of 3 character sequences could produce duplicates.
	 *
	 * On the other hand sequences that are directly tied to the index of the character
	 * resolve a far more accurate measure, it's also faster
	 * to evaluate one condition in a switch statement
	 * than three in an if statement regardless of the added math.
	 *
	 * This allows the vendor prefixer to be both small and fast.
	 */

	var nulptn = /^\0+/g /* matches leading null characters */
	var fmtptn = /[\0\r\f]/g /* matches new line, null and formfeed characters */
	var colonptn = /: */g /* splits animation rules */
	var cursorptn = /zoo|gra/ /* assert cursor varient */
	var transformptn = / *(transform)/g /* vendor prefix transform, older webkit */
	var animationptn = /,+\s*(?![^(]*[)])/g /* splits multiple shorthand notation animations */
	var propertiesptn = / +\s*(?![^(]*[)])/g /* animation properties */
	var elementptn = / *[\0] */g /* selector elements */
	var selectorptn = /,\r+?/g /* splits selectors */
	var andptn = /\f?&/g /* match & */
	var keyptn = /\W+/g /* removes invalid characters from keyframes */
	var escapeptn = /:global\(((?:[^\(\)\[\]]*|\[.*\]|\([^\(\)]*\))*)\)/g /* matches :global(.*) */
	var keyframeptn = /@(k\w+s)\s*(\S*)\s*/ /* matches @keyframes $1 */
	var plcholdrptn = /::?(place)/g /* match ::placeholder varient */
	var beforeptn = /\s+(?=[{\];=:>])/g /* matches \s before ] ; = : */
	var afterptn = /([[}=:>])\s+/g /* matches \s after characters [ } = : */
	var tailptn = /(\{[^{]+?);(?=\})/g /* matches tail semi-colons ;} */
	var whiteptn = /\s{2,}/g /* matches repeating whitespace */
	var pseudoptn = /([^\(])(:+) */g /* pseudo element */

	/* vendors */
	var webkit = '-webkit-'
	var moz = '-moz-'
	var ms = '-ms-'

	/* character codes */
	var SEMICOLON = 59 /* ; */
	var CLOSEBRACES = 125 /* } */
	var OPENBRACES = 123 /* { */
	var OPENPARENTHESES = 40 /* ( */
	var CLOSEPARENTHESES = 41 /* ) */
	var OPENBRACKET = 91 /* [ */
	var CLOSEBRACKET = 93 /* ] */
	var NEWLINE = 10 /* \n */
	var CARRIAGE = 13 /* \r */
	var TAB = 9 /* \t */
	var AT = 64 /* @ */
	var SPACE = 32 /*   */
	var AND = 38 /* & */
	var DASH = 45 /* - */
	var UNDERSCORE = 95 /* _ */
	var STAR = 42 /* * */
	var COMMA = 44 /* , */
	var COLON = 58 /* : */
	var SINGLEQUOTE = 39 /* ' */
	var DOUBLEQUOTE = 34 /* " */
	var FOWARDSLASH = 47 /* / */
	var GREATERTHAN = 62 /* > */
	var PLUS = 43 /* + */
	var TILDE = 126 /* ~ */
	var NULL = 0 /* \0 */
	var FORMFEED = 12 /* \f */
	var VERTICALTAB = 11 /* \v */

	/* special identifiers */
	var KEYFRAME = 107 /* k */
	var MEDIA = 109 /* m */
	var SUPPORTS = 115 /* s */
	var FONT = 102 /* f */
	var PLACEHOLDER = 112 /* p */
	var IMPORT = 169 /* <at>i */
	var CHARSET = 163 /* <at>c */

	var column = 1 /* current column */
	var line = 1 /* current line numebr */
	var pattern = 0 /* :pattern */

	var cascade = 1 /* #id h1 h2 vs h1#id h2#id  */
	var vendor = 1 /* vendor prefix */
	var escape = 1 /* escape :global() pattern */
	var compress = 0 /* compress output */
	var semicolon = 0 /* no/semicolon option */

	/* empty reference */
	var array = []

	/* plugins */
	var plugins = []
	var plugged = 0

	/* plugin context */
	var POSTS = -2
	var PREPS = -1
	var UNKWN = 0
	var PROPS = 1
	var BLCKS = 2
	var ATRUL = 3

	/* plugin newline context */
	var unkwn = 0

	/* keyframe animation */
	var keyed = 1
	var key = ''

	/* selector namespace */
	var nscopealt = ''
	var nscope = ''

	/**
	 * Compile
	 *
	 * @param {Array<string>} parent
	 * @param {Array<string>} current
	 * @param {string} body
	 * @param {number} from
	 * @return {string}
	 */
	function compile (parent, current, body, from) {
		var brq = 0 /* brackets [] */
		var cmt = 0 /* comments /* // or /* */
		var fnq = 0 /* functions () */
		var str = 0 /* quotes '', "" */

		var first = 0 /* first character code */
		var second = 0 /* second character code */
		var code = 0 /* current character code */
		var tail = 0 /* last character code */
		var trail = 0 /* character before last code */
		
		var counter = 0 /* count sequence termination */
		var context = 0 /* track current context */
		var atrule = 0 /* track @at-rule context */
		var pseudo = 0 /* track pseudo token index */
		var caret = 0 /* current character index */
		var fmt = 0 /* control character formating context */
		var insert = 0 /* auto semicolon insertion */
		var invert = 0 /* inverted selector pattern */
		var length = 0 /* generic length address */
		var eof = body.length /* end of file(length) */
		var eol = eof - 1 /* end of file(characters) */

		var char = '' /* current character */
		var chars = '' /* current buffer of characters */
		var child = '' /* next buffer of characters */
		var out = '' /* compiled body */
		var children = '' /* compiled children */
		var flat = '' /* compiled leafs */

		var ref /* generic address */
		var res /* generic address */

		// ...build body
		while (caret < eof) {
			code = body.charCodeAt(caret)

			if (cmt + str + fnq + brq === 0) {
				// auto semicolon insertion
				if (insert === 1) {
					// false flags, comma character
					switch (code) {
						case COMMA: {
							break
						}
						default: {
							caret--
							code = SEMICOLON
						}
					}

					insert = 0
				}

				// eof varient
				if (caret === eol) {
					if (fmt > 0) {
						chars = chars.replace(fmtptn, '')
					}

					if ((chars = chars.trim()).length > 0) {
						switch (code) {
							case SPACE:
							case TAB:
							case SEMICOLON:
							case CARRIAGE:
							case NEWLINE: {
								break
							}
							default: {
								chars += body.charAt(caret)
							}
						}

						code = SEMICOLON
					}
				}

				// token varient
				switch (code) {
					case OPENBRACES: {
						chars = chars.trim()
						first = chars.charCodeAt(0)
						counter = 1
						caret++

						while (caret < eof) {
							code = body.charCodeAt(caret)

							switch (code) {
								case OPENBRACES: {
									counter++
									break
								}
								case CLOSEBRACES: {
									counter--
									break
								}
							}

							if (counter === 0) {
								break
							}

							child += body.charAt(caret++)
						}

						if (first === NULL) {
							first = (chars = chars.replace(nulptn, '').trim()).charCodeAt(0)
						}

						switch (first) {
							// @at-rule
							case AT: {
								if (fmt > 0) {
									chars = chars.replace(fmtptn, '')
								}

								second = chars.charCodeAt(1)
								child = compile(current, second > 108 ? current : array, child, second)

								// execute plugins, @at-rule context
								if (plugged > 0) {
									ref = select(array, chars, invert)
									res = proxy(ATRUL, child, ref, current, line, column, out.length)
									chars = ref.join('')

									if (res !== void 0) {
										child = res
									}
								}

								switch (second) {
									case MEDIA:
									case SUPPORTS: {
										child = chars + '{' + child + '}'
										break
									}
									case FONT: {
										child = chars + child
										break
									}
									case KEYFRAME: {
										chars = chars.replace(keyframeptn, '$1 $2' + (keyed > 0 ? key : ''))
										child = chars + '{' + child + '}'
										child = '@' + (vendor > 0 ? webkit + child + '@' + child : child)
										break
									}
								}
								break
							}
							// selector
							default: {
								child = compile(current, select(current, chars, invert), child, from)
							}
						}

						children += child
						caret++

						// reset
						context = 0
						pseudo = 0
						fmt = 0
						invert = 0
						atrule = 0
						chars = ''
						child = ''

						break
					}
					case CLOSEBRACES:
					case SEMICOLON: {
						chars = (fmt > 0 ? chars.replace(fmtptn, '') : chars).trim()
						
						if (code !== CLOSEBRACES || chars.length > 0) {
							// execute plugins, property context
							if (plugged > 0) {
								if ((res = proxy(PROPS, chars, current, parent, line, column, out.length)) !== void 0) {
									if ((chars = res.trim()).length === 0) {
										chars = '\0\0'
									}
								}
							}

							first = chars.charCodeAt(0)
							second = chars.charCodeAt(1)

							switch (first + second) {
								case NULL: {
									break
								}
								case IMPORT:
								case CHARSET: {
									flat += chars + body.charAt(caret)
									break
								}
								default: {
									out += pseudo > 0 ? property(chars, first, second, chars.charCodeAt(2)) : chars+';'
								}
							}
						}

						caret++

						// reset
						context = 0
						pseudo = 0
						fmt = 0
						invert = 0
						chars = ''

						break
					}
				}
			}

			// parse characters
			switch (code) {
				case CARRIAGE:
				case NEWLINE: {
					// auto insert semicolon
					if (cmt + str + fnq + brq + semicolon === 0) {
						// valid characters that
						// may precede a newline
						switch (tail) {
							case COLON:
							case COMMA:
							case NULL:
							case TAB:
							case CARRIAGE:
							case NEWLINE:
							case SPACE:
							case SEMICOLON:
							case OPENBRACES:
							case CLOSEBRACES: {
								break
							}
							default: {
								// colon : present? register for auto semicolon insertion
								if (pseudo > 0) {
									insert = 1
								}
							}
						}
					}

					// terminate line comment
					if (cmt === FOWARDSLASH) {
						cmt = 0
					}

					// execute plugins, newline context
					if (plugged * unkwn > 0) {
						proxy(UNKWN, chars, current, parent, line, column, out.length)
					}

					// next line, reset column position
					column = 1
					line++

					break
				}
				default: {
					// increment column position
					column++

					// ignore tabs
					if (code === TAB) {
						break
					}

					// current character
					char = body.charAt(caret)

					// remove comments, escape functions, strings, attributes and prepare selectors
					switch (code) {
						// escape breaking control characters
						case NULL: {
							char = '\\0'
							break
						}
						case FORMFEED: {
							char = '\\f'
							break
						}
						case VERTICALTAB: {
							char = '\\v'
							break
						}
						// &
						case AND: {
							// inverted selector pattern i.e html &
							if (str + cmt + brq === 0 && cascade*tail === SPACE) {
								invert = 1
								fmt = 1
								char = '\f' + char
							}
							break
						}
						// :p<l>aceholder
						// :g<l>obal
						case 108: {
							if (str + cmt + brq + pattern === 0 && pseudo > 0 && caret - pseudo === 2) {
								if (tail !== PLACEHOLDER || body.charCodeAt(caret-3) === COLON) {
									pattern = tail
								}
							}
							break
						}
						// :<pattern>
						case COLON: {
							if (str + cmt + brq === 0) {
								pseudo = caret
							}
							break
						}
						// selectors
						case COMMA: {
							if (cmt + fnq + str + brq === 0) {
								invert = 1
								fmt = 1
								char += '\r'
							}
							break
						}
						// qoutes
						case DOUBLEQUOTE: {
							if (cmt === 0) {
								str = str === DOUBLEQUOTE ? 0 : (str === 0 ? DOUBLEQUOTE : str)
							}
							break
						}
						case SINGLEQUOTE: {
							if (cmt === 0) {
								str = str === SINGLEQUOTE ? 0 : (str === 0 ? SINGLEQUOTE : str)
							}
							break
						}
						// attributes
						case OPENBRACKET: {
							if (str + cmt + fnq === 0) {
								brq = 1
							}
							break
						}
						case CLOSEBRACKET: {
							if (str + cmt + fnq === 0) {
								brq = 0
							}
							break
						}
						// functions
						case CLOSEPARENTHESES: {
							if (str + cmt + brq === 0) {
								// ) is the last character, add synthetic padding to avoid skipping this buffer
								if (caret === eol) {
									eol++
									eof++
								}
								fnq--
							}
							break
						}
						case OPENPARENTHESES: {
							if (str + cmt + brq === 0) {
								if (context === 0) {
									switch (tail*2 + trail*3) {
										// :matches
										case 533: {
											break
										}
										// :global, :not, :nth-child etc...
										default: {
											counter = 0
											context = 1
										}
									}
								}
								fnq++
							}
							break
						}
						case AT: {
							if (cmt + fnq + str + brq + pseudo + atrule === 0) {
								atrule = 1
							}
							break
						}
						// block/line comments
						case STAR:
						case FOWARDSLASH: {
							if (str + brq + fnq > 0) {
								break
							}

							switch (cmt) {
								// initialize line/block comment context
								case 0: {
									switch (code*2 + body.charCodeAt(caret+1)*3) {
										// //
										case 235: {
											cmt = FOWARDSLASH
											break
										}
										// /*
										case 220: {
											cmt = STAR
											break
										}
									}
									break
								}
								// end block comment context
								case STAR: {
									if (code === FOWARDSLASH && tail === STAR) {
										char = ''
										cmt = 0
									}
								}
							}
						}
					}

					// ignore comment blocks
					if (cmt === 0) {
						// aggressive isolation mode, divide each individual selector
						// including selectors in :not function but excluding selectors in :global function
						if (cascade + str + brq + atrule === 0 && from !== KEYFRAME && code !== SEMICOLON) {
							switch (code) {
								case COMMA:
								case TILDE:
								case GREATERTHAN:
								case PLUS:
								case CLOSEPARENTHESES:
								case OPENPARENTHESES: {
									if (context === 0) {
										// outside of an isolated context i.e nth-child(<...>)
										switch (tail) {
											case TAB:
											case SPACE:
											case NEWLINE:
											case CARRIAGE: {
												char = char + '\0'
												break
											}
											default: {
												char = '\0' + char + (code === COMMA ? '' : '\0')
											}
										}
										fmt = 1
									} else {
										// within an isolated context, sleep untill it's terminated
										switch (code) {
											case OPENPARENTHESES: {
												context = ++counter
												break
											}
											case CLOSEPARENTHESES: {
												if ((context = --counter) === 0) {
													fmt = 1
													char += '\0'
												}
												break
											}
										}
									}
									break
								}
								case SPACE: {
									switch (tail) {
										case NULL:
										case OPENBRACES:
										case CLOSEBRACES:
										case SEMICOLON:
										case COMMA:
										case FORMFEED:
										case TAB:
										case SPACE:
										case NEWLINE:
										case CARRIAGE: {
											break
										}
										default: {
											// ignore in isolated contexts
											if (context === 0) {
												fmt = 1
												char += '\0'
											}
										}
									}
								}
							}
						}

						// concat buffer of characters
						chars += char
					}
				}
			}

			// tail character codes
			trail = tail
			tail = code

			// visit every character
			caret++
		}

		length = out.length

		// execute plugins, block context
		if (length > 0 && plugged > 0) {
			res = proxy(BLCKS, out, current, parent, line, column, length)

			if (res !== void 0) {
				length = (out = res).length
			}
		}

		if (length > 0) {
			// cascade isolation mode
			if (cascade === 0 && from !== KEYFRAME) {
				isolate(current)
			}

			out = current.join(',').trim() + '{' + out + '}'

			switch (pattern) {
				case 0: {
					break
				}
				case PLACEHOLDER: {
					// ::placeholder vendor prefix
					if (vendor > 0) {
						out = (out.replace(plcholdrptn, '::' + webkit + 'input-$1') +
							out.replace(plcholdrptn, '::' + moz + '$1') +
							out.replace(plcholdrptn, ':' + ms + 'input-$1') + out)
					}
				}
				default: {
					pattern = 0
				}
			}
		}

		return flat + out + children
	}

	/**
	 * Select
	 *
	 * @param {Array<string>} parent
	 * @param {string} current
	 * @param {number} invert
	 * @return {Array<string>}
	 */
	function select (parent, current, invert) {
		var selectors = current.trim().split(selectorptn)
		var out = selectors

		var length = selectors.length
		var l = parent.length

		switch (l) {
			// 0-1 parent selectors
			case 0:
			case 1: {
				for (var i = 0, ref = l === 0 ? '' : parent[0] + ' '; i < length; i++) {
					out[i] = scope(out[i], ref, l, invert).trim()
				}
				break
			}
			// >2 parent selectors, nested
			default: {
				for (var i = 0, j = 0, out = []; i < length; i++) {
					for (var k = 0; k < l; k++) {
						out[j++] = scope(selectors[i], parent[k] + ' ', l, invert).trim()
					}
				}
			}
		}

		return out
	}

	/**
	 * Scope
	 *
	 * @param {string} input
	 * @param {string} parent
	 * @param {number} level
	 * @param {number} invert
	 * @return {string}
	 */
	function scope (input, parent, level, invert) {
		var selector = input
		var prefix = parent
		var code = selector.charCodeAt(0)

		// trim leading whitespace
		if (code < 33) {
			code = (selector = selector.trim()).charCodeAt(0)
		}

		switch (code) {
			// &
			case AND: {
				switch (cascade + level) {
					case 0:
					case 1: {
						if (parent.trim().length === 0) {
							break
						}
					}
					default: {
						return selector.replace(andptn, prefix.trim())
					}
				}
				break
			}
			// :
			case COLON: {
				switch (selector.charCodeAt(1)) {
					// g in :global
					case 103: {
						if (escape > 0 && cascade > 0) {
							return selector.replace(escapeptn, '$1').replace(andptn, nscope)
						}
						break
					}
					default: {
						// :hover
						return prefix.trim() + selector
					}
				}
			}
			default: {
				// html &
				if (invert*cascade > 0 && (code = selector.indexOf('\f')) > 0) {
					return (
						prefix.replace(nscope, '').trim() + ' ' + 
						selector.substring(0, code) + nscope + selector.substring(code+2)
					)
				}
			}
		}

		return prefix + selector
	}

	/**
	 * Property
	 *
	 * @param {string} input
	 * @param {number} first
	 * @param {number} second
	 * @param {number} third
	 * @return {string}
	 */
	function property (input, first, second, third) {
		var out = input + ';'
		var index = 0
		var hash = (first*2) + (second*3) + (third*4)
		var cache

		// animation: a, n, i characters
		if (hash === 944) {
			out = animation(out)
		} else if (vendor > 0) {
			// vendor prefix
			switch (hash) {
				// background/backface-visibility, b, a, c
				case 883: {
					// backface-visibility, -
					if (out.charCodeAt(8) === DASH) {
						out = webkit + out + out
					}
					break
				}
				// appearance: a, p, p
				case 978: {
					out = webkit + out + moz + out + out
					break
				}
				// hyphens: h, y, p
				// user-select: u, s, e
				case 1019:
				case 983: {
					out = webkit + out + moz + out + ms + out + out
					break
				}
				// flex: f, l, e
				case 932: {
					out = webkit + out + ms + out + out
					break
				}
				case 964: {
					// order: o, r, d
					out = webkit + out + ms + 'flex' + '-' + out + out
					break
				}
				// justify-content, j, u, s
				case 1023: {
					cache = out.substring(out.indexOf(':', 15)).replace('flex-', '')
					out = webkit + 'box-pack' + cache + webkit + out + ms + 'flex-pack' + cache + out
					break
				}
				// display(flex/inline-flex/inline-box): d, i, s
				case 975: {
					index = (out = input).length-10
					cache = (out.charCodeAt(index) === 33 ? out.substring(0, index) : out).substring(8).trim()

					switch (hash = cache.charCodeAt(0) + (cache.charCodeAt(7)|0)) {
						// inline-
						case 203: {
							// inline-box
							if (cache.charCodeAt(8) > 110) {
								out = out.replace(cache, webkit+cache)+';'+out
							}
							break
						}
						// inline-flex
						// flex
						case 207:
						case 102: {
							out = (
								out.replace(cache, webkit+(hash > 102 ? 'inline-' : '')+'box')+';'+
								out.replace(cache, webkit+cache)+';'+
								out.replace(cache, ms+cache+'box')+';'+
								out
							)
						}
					}
					
					out += ';'
					break
				}
				// align-items, align-center, align-self: a, l, i, -
				case 938: {
					if (out.charCodeAt(5) === DASH) {
						switch (out.charCodeAt(6)) {
							// align-items, i
							case 105: {
								cache = out.replace('-items', '')
								out = webkit + out + webkit + 'box-' + cache + ms + 'flex-' + cache + out
								break
							}
							// align-self, s
							case 115: {
								out = webkit + out + ms + 'flex-item-' + out.replace('-self', '') + out
								break
							}
							// align-content
							default: {
								out = webkit + out + ms + 'flex-line-pack' + out.replace('align-content', '') + out
							}
						}
					}
					break
				}
				// cursor, c, u, r
				case 1005: {
					if (cursorptn.test(out)) {
						out = out.replace(colonptn, ': ' + webkit) + out.replace(colonptn, ': ' + moz) + out
					}
					break
				}
				// width: min-content / width: max-content
				case 953: {
					if ((index = out.indexOf('-content', 9)) > 0) {
						// width: min-content / width: max-content
						cache = out.substring(index - 3)
						out = 'width:' + webkit + cache + 'width:' + moz + cache + 'width:' + cache
					}
					break
				}
				// transform, transition: t, r, a
				// text-size-adjust: t, e, x
				case 962:
				case 1015: {
					out = webkit + out + (out.charCodeAt(5) === 102 ? ms + out : '') + out

					// transitions
					if (second + third === 211 && out.charCodeAt(13) === 105 && out.indexOf('transform', 10) > 0) {
						out = out.substring(0, out.indexOf(';', 27) + 1).replace(transformptn, webkit + '$1') + out
					}

					break
				}
			}
		}

		return out
	}

	/**
	 * Animation
	 *
	 * @param {string} input
	 * @return {string}
	 */
	function animation (input) {
		var length = input.length
		var index = input.indexOf(':', 9) + 1
		var declare = input.substring(0, index).trim()
		var body = input.substring(index, length-1).trim()
		var out = ''

		// shorthand
		if (input.charCodeAt(9) !== DASH) {
			// split in case of multiple animations
			var list = body.split(animationptn)

			for (var i = 0, index = 0, length = list.length; i < length; index = 0, i++) {
				var value = list[i]
				var items = value.split(propertiesptn)

				while (value = items[index]) {
					var peak = value.charCodeAt(0)

					if (keyed === 1 && (
						// letters
						(peak > AT && peak < 90) || (peak > 96 && peak < 122) || peak === UNDERSCORE ||
						// dash but not in sequence i.e --
						(peak === DASH && value.charCodeAt(1) !== DASH)
					)) {
						// not a number/function
						switch (isNaN(parseFloat(value)) + (value.indexOf('(') !== -1)) {
							case 1: {
								switch (value) {
									// not a valid reserved keyword
									case 'infinite': case 'alternate': case 'backwards': case 'running':
									case 'normal': case 'forwards': case 'both': case 'none': case 'linear':
									case 'ease': case 'ease-in': case 'ease-out': case 'ease-in-out':
									case 'paused': case 'reversed': case 'alternate-reverse': case 'inherit':
									case 'initial': case 'unset': case 'step-start': case 'step-end': {
										break
									}
									default: {
										value += key
									}
								}
							}
						}
					}

					items[index++] = value
				}

				out += (i === 0 ? '' : ',') + items.join(' ')
			}
		} else {
			// animation-name, n
			out += input.charCodeAt(10) === 110 ? body + (keyed === 1 ? key : '') : body
		}

		out = declare + out + ';'

		return vendor > 0 ? webkit + out + out : out
	}

	/**
	 * Isolate
	 *
	 * @param {Array<string>} selectors
	 */
	function isolate (selectors) {
		for (var i = 0, length = selectors.length, padding, element; i < length; i++) {
			// split individual elements in a selector i.e h1 h2 === [h1, h2]
			var elements = selectors[i].split(elementptn)
			var out = ''

			for (var j = 0, size = 0, tail = 0, code = 0, l = elements.length; j < l; j++) {
				// empty element
				if ((size = (element = elements[j]).length) === 0 && l > 1) {
					continue
				}

				tail = out.charCodeAt(out.length-1)
				code = element.charCodeAt(0)
				padding = ''

				if (j !== 0) {
					// determine if we need padding
					switch (tail) {
						case STAR:
						case TILDE:
						case GREATERTHAN:
						case PLUS:
						case SPACE:
						case OPENPARENTHESES:  {
							break
						}
						default: {
							padding = ' '
						}
					}
				}

				switch (code) {
					case AND: {
						element = padding + nscopealt
					}
					case TILDE:
					case GREATERTHAN:
					case PLUS:
					case SPACE:
					case CLOSEPARENTHESES:
					case OPENPARENTHESES: {
						break
					}
					case OPENBRACKET: {
						element = padding + element + nscopealt
						break
					}
					case COLON: {
						switch (element.charCodeAt(1)*2 + element.charCodeAt(2)*3) {
							// :global
							case 530: {
								if (escape > 0) {
									element = padding + element.substring(8, size - 1)
									break
								}
							}
							// :hover, :nth-child(), ...
							default: {
								if (j < 1 || elements[j-1].length < 1) {
									element = padding + nscopealt + element
								}
							}
						}
						break
					}
					case COMMA: {
						padding = ''
					}
					default: {
						if (size > 1 && element.indexOf(':') > 0) {
							element = padding + element.replace(pseudoptn, '$1' + nscopealt + '$2')
						} else {
							element = padding + element + nscopealt
						}
					}
				}

				out += element
			}

			selectors[i] = out.replace(fmtptn, '').trim()
		}
	}

	/**
	 * Proxy
	 *
	 * @param {number} context
	 * @param {string} content
	 * @param {Array<string>} selectors
	 * @param {Array<string>} parents
	 * @param {number} line
	 * @param {number} column
	 * @param {number} length
	 * @return {(string|void|*)}
	 */
	function proxy (context, content, selectors, parents, line, column, length) {
		for (var i = 0, out = content, next; i < plugged; i++) {
			switch (next = plugins[i].call(stylis, context, out, selectors, parents, line, column, length)) {
				case void 0:
				case false:
				case true:
				case null: {
					break
				}
				default: {
					out = next
				}
			}
		}

		switch (out) {
			case void 0:
			case false:
			case true:
			case null:
			case content: {
				break
			}
			default: {
				return out
			}
		}
	}

	/**
	 * Minify
	 *
	 * @param {(string|*)} output
	 * @return {string}
	 */
	function minify (output) {
		return output
			.replace(fmtptn, '')
			.replace(beforeptn, '')
			.replace(afterptn, '$1')
			.replace(tailptn, '$1')
			.replace(whiteptn, ' ')
	}

	/**
	 * Use
	 *
	 * @param {(Array<function(...?)>|function(...?)|number|void)?} plugin
	 */
	function use (plugin) {
		switch (plugin) {
			case void 0:
			case null: {
				plugged = plugins.length = 0
				break
			}
			default: {
				switch (plugin.constructor) {
					case Array: {
						for (var i = 0, length = plugin.length; i < length; i++) {
							use(plugin[i])
						}
						break
					}
					case Function: {
						plugins[plugged++] = plugin
						break
					}
					case Boolean: {
						unkwn = !!plugin|0
					}
				}
			}
 		}

 		return use
	}

	/**
	 * Set
	 *
	 * @param {Object} options
	 */
	function set (options) {
		for (var name in options) {
			var value = options[name]
			switch (name) {
				case 'keyframe': keyed = value|0; break
				case 'global': escape = value|0; break
				case 'cascade': cascade = value|0; break
				case 'compress': compress = value|0; break
				case 'prefix': vendor = value|0; break
				case 'semicolon': semicolon = value|0; break
			}
		}

		return set
	}

	/**
	 * Stylis
	 *
	 * @param {string} selector
	 * @param {string} input
	 * @return {(string|*)}
	 */
	function stylis (selector, input) {
		// setup
		var ns = selector
		var code = ns.charCodeAt(0)

		// trim leading whitespace
		if (code < 33) {
			code = (ns = ns.trim()).charCodeAt(0)
		}

		// keyframe/animation namespace
		if (keyed > 0) {
			key = ns.replace(keyptn, code === OPENBRACKET ? '' : '-')
		}

		// reset, used to assert if a plugin is moneky-patching the return value
		code = 1

		// cascade/isolate
		if (cascade === 1) {
			nscope = ns
		} else {
			nscopealt = ns
		}

		var selectors = [nscope]

		// execute plugins, pre-process context
		if (plugged > 0) {
			proxy(PREPS, input, selectors, selectors, line, column, 0)
		}

		// build
		var output = compile(array, selectors, input, 0)

		// execute plugins, post-process context
		if (plugged > 0) {
			var res = proxy(POSTS, output, selectors, selectors, line, column, output.length)
			
			if (res !== void 0) {
				// bypass minification
				if (typeof(output = res) !== 'string') {
					code = 0
				}
			}
		}

		// reset
		key = ''
		nscope = ''
		nscopealt = ''
		pattern = 0
		line = 1
		column = 1

		return compress*code === 0 ? output : minify(output)
	}

	stylis['use'] = use
	stylis['set'] = set

	return stylis
}));
