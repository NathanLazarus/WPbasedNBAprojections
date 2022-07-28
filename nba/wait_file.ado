cap program drop wait_file
program def wait_file

//cap {
	local i = 1
	while "``i''" != "" {
		local to_erase = "`to_erase' `i'done.txt"
		* confirm file `i'done.txt
		local ++i
	}
//}
local return = 1
while `return'>0 {
	sleep 400
	local j = 1
	local return = 0
	while "``j''" != "" {
		cap confirm file ``j''done.txt
		local return = `return'+_rc
		local ++j
	}

}

end
