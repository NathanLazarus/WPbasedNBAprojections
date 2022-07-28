forvalues file=1/2 {
	qui import delim using ./millionsims/playoff`file'.csv
	gen firstroundopploc = 9-seed*2
	bysort simnumber west seed: gen firstround_team=team[]
}
