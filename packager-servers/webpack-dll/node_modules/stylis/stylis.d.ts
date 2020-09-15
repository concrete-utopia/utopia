type selectors = Array<string>

declare enum context {
	POSTS = -2,
	PREPS = -1,
	UNKWN = 0,
	PROPS = 1,
	BLCKS = 2,
	ATRUL = 3
}

interface options {
	keyframe?: boolean
	global?: boolean
	cascade?: boolean
	compress?: boolean
	prefix?: boolean
	semicolon?: boolean
}

interface set {
	(options?: options): set
}

interface plugin {
	(
		this: stylis,
		context: context, 
		content: string, 
		selector: selectors, 
		parent: selectors, 
		line: number, 
		column: number, 
		length: number
	): null|void|string
}

interface use {
	(plugin?: Array<plugin>|plugin|null): use
}

interface stylis {
	(namescope: string, input: string): string|any
	set: set
	use: use
}

declare global {
	export const stylis: stylis
}

export = stylis
