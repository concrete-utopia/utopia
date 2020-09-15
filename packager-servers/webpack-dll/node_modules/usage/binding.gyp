{
	'targets': [
		{
			'target_name': 'sysinfo',
			'conditions': [
				['OS=="solaris"', {
					'sources': [
						'src/solaris.cpp'
					]
				}]
			],
			"include_dirs": [
				"<!(node -e \"require('nan')\")"
			],
			'sources': [
				'src/binding.cpp',
			],
			'linkflags': [
				'-Lbuild/cd Release/obj.target/sysinfo/src/'
			],
			'defines': [
				'OS="<(OS)"',
				'is_<(OS)'
			],
		}
	]
}
