bspid_instantiation = function() {
  c(
    
    ### 002
    B002 = methods::new(
      Class = "BSPID", 
      bspid = "002", 
      groups = list(
        spalling = c(
          "002", "002-01", "002-02", "002-03", "002-04", "002-05", "002-06", 
          "002-07", "002-08"
        ),
        spalling_spec = c(
          "002", "002-02", "002-03", "002-04"
        ),
        moisture = c(
          "002", "002-09", "002-10"
        )
      )
    ),
    
    ### 006
    B006 = methods::new(
      Class = "BSPID", 
      bspid = "006", 
      groups = list(
        damaged = c(
          "006-01", "006-01-01", "006-01-02", "006-01-03", "006-01-04", 
          "006-01-05", "006-01-06", "006-02", "006-02-01", "006-02-02", 
          "006-02-03", "006-02-04", "006-02-05", "006-02-06", "006-02-07", 
          "006-03", "006-03-01", "006-03-02", "006-03-03", "006-03-04", 
          "006-03-05", "006-03-06", "006-03-07"
        )
      )
    ),
    
    ### 006-01
    B006_01 = methods::new(
      Class = "BSPID", 
      bspid = "006-01", 
      groups = list(
        near_surface = c(
          "006-01", "006-01-01", "006-01-02", "006-01-03", "006-01-04", 
          "006-01-05", "006-01-06" 
        ),
        near_surface_nospray = c(
          "006-01", "006-01-01", "006-01-03", "006-01-05"
        ),
        near_surface_spray = c(
          "006-01", "006-01-02", "006-01-04", "006-01-06"
        )
      )
    ),
    
    ### 006-02
    B006_02 = methods::new(
      Class = "BSPID", 
      bspid = "006-02", 
      groups = list(
        crack_sm04 = c(
          "006-02", "006-02-01", "006-02-02", "006-02-03", "006-02-04", 
          "006-02-05", "006-02-06"
        ),
        crack_sm04_nospray = c(
          "006-02", "006-02-01", "006-02-03", "006-02-05"
        ),
        crack_sm04_spray = c(
          "006-02", "006-02-02", "006-02-04", "006-02-06"
        )
      )
    ),
    
    ### 006-03
    B006_03 = methods::new(
      Class = "BSPID", 
      bspid = "006-03", 
      groups = list(
        crack_gr04 = c(
          "006-03", "006-03-01", "006-03-02", "006-03-03", "006-03-04", 
          "006-03-05", "006-03-06"
        ),
        crack_gr04_nospray = c(
          "006-03", "006-03-01", "006-03-03", "006-03-05"
        ),
        crack_gr04_spray = c(
          "006-03", "006-03-02", "006-03-04", "006-03-06"
        )
      )
    ),
    
    ### 021
    B021 = methods::new(
      Class = "BSPID", 
      bspid = "021", 
      groups = list(
        spalling = c(
          "021", "021-07", "021-08", "021-09", "021-10", "021-11", "021-12"
        ),
        moisture = c(
          "021", "021-05", "021-06"
        )
      )
    ),
    
    ### 025
    B025 = methods::new(
      Class = "BSPID", 
      bspid = "025", 
      groups = list(
        crack = c(
          "025", "025-01", "025-02", "025-03", "025-04", "025-05", "025-06", 
          "025-07", "025-08"
        ),
        crack_dry = c(
          "025", "025-01", "025-02", "025-03", "025-04", "025-05", "025-06"
        ),
        crack_dry_nospray = c(
          "025", "025-01", "025-03", "025-05"
        ),
        crack_dry_spray = c(
          "025", "025-02", "025-04", "025-06"
        ),
        crack_wet = c(
          "025", "025-07", "025-08"
        )
        
      )
    ),
    
    ### 230
    B230 = methods::new(
      Class = "BSPID", 
      bspid = "230", 
      groups = list(
        crack = c(
          "230", "230-02", "230-04"
        ),
        hole = c(
          "230", "230-08", "230-09", "230-10", "230-11", "230-12", "230-20"
        )
      )
    ),
    
    ### 233
    B233 = methods::new(
      Class = "BSPID", 
      bspid = "233", 
      groups = list(
        loose_kerb = c(
          "233", "233-03", "233-04", "233-05", "233-06", "233-07", "233-08", 
          "233-09"
        )
      )
    ),
    
    ### 241
    B241 = methods::new(
      Class = "BSPID", 
      bspid = "241", 
      groups = list(
        cracked_surface = c(
          "241", "241-04", "241-05", "241-06", "241-07", "241-08", "241-09", 
          "241-13", "241-15", "241-16"
        )
      )
    ),
    
    ### 259
    B259 = methods::new(
      Class = "BSPID", 
      bspid = "259", 
      groups = list(
        damaged_joint = c(
          "259", "259-01", "259-02", "259-03", "259-06"
        )
      )
    ),
    
    ### 027
    B027 = methods::new(
      Class = "BSPID", 
      bspid = "027", 
      groups = list(
        spalling = c(
          "027", "027-01", "027-02", "027-03", "027-04", "027-06", "027-09", 
          "027-10", "027-11", "027-12", "027-13"
        ),
        moisture = c(
          "027", "027-07", "027-08"
        )
      )
    ),
    
    ### 231
    B231 = methods::new(
      Class = "BSPID", 
      bspid = "231", 
      groups = list(
        damaged_post = c(
          "231", "231-14", "231-15", "231-16", "231-17", "231-20", "231-21",
          "231-22", "231-23"
        )
      )
    ),
    
    ### 258
    B258 = methods::new(
      Class = "BSPID", 
      bspid = "258", 
      groups = list(
        damaged = c(
          "258", "258-01", "258-02", "258-03"
        )
      )
    ),
    
    ### 234
    B234 = methods::new(
      Class = "BSPID", 
      bspid = "234", 
      groups = list(
        damaged = c(
          "234", "234-01", "234-02", "234-03", "234-04", "234-05", "234-09"
        )
      )
    ),
    
    ### 236
    B236 = methods::new(
      Class = "BSPID", 
      bspid = "236", 
      groups = list(
        damaged = c(
          "236", "236-01", "236-03", "236-04", "236-07", "236-08", "236-09", 
          "236-10"
        )
      )
    ),
    
    ### 251
    B251 = methods::new(
      Class = "BSPID", 
      bspid = "251", 
      groups = list(
        sediment = c(
          "251", "251-01", "251-02", "251-03", "251-04" , "251-05", "251-06", 
          "251-07"
        ),
        sediment_deposition = c(
          "251", "251-01", "251-03", "251-05", "251-06", "251-07"
        ),
        sediment_erosion = c(
          "251", "251-02", "251-04", "251-05", "251-06", "251-07"
        )
      )
    ),
    
    ### 009
    B009 = methods::new(
      Class = "BSPID", 
      bspid = "009", 
      groups = list(
        spalling = c(
          "009", "009-01", "009-02", "009-03", "009-05", "009-07", "009-08", 
          "009-11", "009-12", "009-14", "009-15", "009-16", "009-17"
        ),
        moisture = c(
          "009", "009-09", "009-10"
        )
      )
    ),
    
    ### 237
    B237 = methods::new(
      Class = "BSPID", 
      bspid = "237", 
      groups = list(
        crack = c(
          "237", "237-02", "237-03"
        ),
        spalling = c(
          "237", "237-06", "237-07", "237-08", "237-09", "237-10", "237-11", "237-13"
        )

      )
    ),
    
    ### 253
    B253 = methods::new(
      Class = "BSPID", 
      bspid = "253", 
      groups = list()
    ),
    
    ### 020
    B020 = methods::new(
      Class = "BSPID", 
      bspid = "020", 
      groups = list(
        soiling = c(
          "020", "020-01", "020-02", "020-04", "020-05"
        )
      )
    ),
    
    ### 010
    B010 = methods::new(
      Class = "BSPID", 
      bspid = "010", 
      groups = list(
        crack = c(
          "010", "010-01", "010-04", "010-07"
        )
      )
    ),
    
    ### 232
    B232 = methods::new(
      Class = "BSPID", 
      bspid = "232", 
      groups = list(
        damaged = c(
          "232", "232-12", "232-13", "232-14", "232-16"
        )
      )
    ),
    
    ### 001
    B001 = methods::new(
      Class = "BSPID", 
      bspid = "001", 
      groups = list(
        soiling = c(
          "001", "001-01", "001-02", "001-05"
        )
      )
    ),
    
    ### 214
    B214 = methods::new(
      Class = "BSPID", 
      bspid = "214", 
      groups = list(
        damaged = c(
          "214", "214-04", "214-06"
        ),
        corrosion = c(
          "214", "214-08", "214-09"
        )
      )
    ),
    
    ### 244
    B244 = methods::new(
      Class = "BSPID", 
      bspid = "244", 
      groups = list(
        damaged = c(
          "244", "244-01", "244-02", "244-04", "244-05", "244-06", "244-07", 
          "244-08", "244-09", "244-10"
        ),
        spalling = c(
          "244", "244-06", "244-07", "244-08"
        ),
        crack = c(
          "244", "244-09", "244-10"
        )
      )
    ),
    
    ### 252
    B252 = methods::new(
      Class = "BSPID", 
      bspid = "252", 
      groups = list()
    ),
    
    ### 257
    B257 = methods::new(
      Class = "BSPID", 
      bspid = "257", 
      groups = list(
        damaged = c(
          "257", "257-01", "257-02", "257-03", "257-04"
        )
      )
    ),
    
    ### 226
    B226 = methods::new(
      Class = "BSPID", 
      bspid = "226", 
      groups = list(
        soiling = c(
          "226", "226-01", "226-02", "226-03", "226-04"
        ),
        corrosion = c(
          "226", "226-01", "226-02", "226-03", "226-04"
        )
      )
    ),
    
    ### 225
    B225 = methods::new(
      Class = "BSPID", 
      bspid = "225", 
      groups = list(
        damaged = c(
          "225", "225-01", "225-02", "225-03"
        )
      )
    ),
    
    ### 003
    B003 = methods::new(
      Class = "BSPID", 
      bspid = "003", 
      groups = list()
    ),
    
    ### 022
    B022 = methods::new(
      Class = "BSPID", 
      bspid = "022", 
      groups = list()
    ),
    
    ### 213
    B213 = methods::new(
      Class = "BSPID", 
      bspid = "213", 
      groups = list()
    ),
    
    ### 261
    B261 = methods::new(
      Class = "BSPID", 
      bspid = "261", 
      groups = list()
    ),
    
    ### 312
    B312 = methods::new(
      Class = "BSPID", 
      bspid = "312", 
      groups = list()
    ),
    
    ### 309
    B309 = methods::new(
      Class = "BSPID", 
      bspid = "309", 
      groups = list()
    ),
    
    ### 014
    B014 = methods::new(
      Class = "BSPID", 
      bspid = "014", 
      groups = list()
    ),
    
    ### 250
    B250 = methods::new(
      Class = "BSPID", 
      bspid = "250", 
      groups = list()
    ),
    
    ### 030
    B030 = methods::new(
      Class = "BSPID", 
      bspid = "030", 
      groups = list()
    ),
    
    ### 201
    B201 = methods::new(
      Class = "BSPID", 
      bspid = "201", 
      groups = list()
    ),
    
    ### 031
    B031 = methods::new(
      Class = "BSPID", 
      bspid = "031", 
      groups = list()
    ),
    
    ### 223
    B223 = methods::new(
      Class = "BSPID", 
      bspid = "223", 
      groups = list()
    ),
    
    ### 075
    B075= methods::new(
      Class = "BSPID", 
      bspid = "075", 
      groups = list()
    ),
    
    ### 075-01
    B075_01 = methods::new(
      Class = "BSPID", 
      bspid = "075-01", 
      groups = list()
    ),
    
    ### 075-02
    B075_02 = methods::new(
      Class = "BSPID", 
      bspid = "075-02", 
      groups = list()
    ),
    
    ### 254
    B254 = methods::new(
      Class = "BSPID", 
      bspid = "254", 
      groups = list()
    ),
    
    ### 016
    B016 = methods::new(
      Class = "BSPID", 
      bspid = "016", 
      groups = list()
    ),
    
    ### 013
    B013 = methods::new(
      Class = "BSPID", 
      bspid = "013", 
      groups = list()
    ),
    
    ### 224
    B224 = methods::new(
      Class = "BSPID", 
      bspid = "224", 
      groups = list()
    ),
    
    ### 011
    B011 = methods::new(
      Class = "BSPID", 
      bspid = "011", 
      groups = list()
    ),
    
    ### 033
    B033 = methods::new(
      Class = "BSPID", 
      bspid = "033", 
      groups = list()
    ),
    
    ### 203
    B203 = methods::new(
      Class = "BSPID", 
      bspid = "203", 
      groups = list()
    ),
    
    ### 210
    B210 = methods::new(
      Class = "BSPID", 
      bspid = "210", 
      groups = list()
    ),
    
    ### 012
    B012 = methods::new(
      Class = "BSPID", 
      bspid = "012", 
      groups = list()
    ),
    
    ### 051
    B051 = methods::new(
      Class = "BSPID", 
      bspid = "051", 
      groups = list()
    ),
    
    ### 055-02
    B055_02 = methods::new(
      Class = "BSPID", 
      bspid = "055-02", 
      groups = list()
    ),
    
    ### 211
    B211 = methods::new(
      Class = "BSPID", 
      bspid = "211", 
      groups = list()
    )
    
  )
}


