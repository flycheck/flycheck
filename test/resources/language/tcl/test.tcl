#!tclsh

set vals {1 2 3}

foreach val $vals {
	puts $val
	set tmp [expr $val * 3]
	set val_${val} $tmp
	puts [set val_${val}]
}

set tmp 1 2 3
