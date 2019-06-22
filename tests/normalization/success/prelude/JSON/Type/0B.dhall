  λ(JSON : Type)
→ λ ( json
	: { array :
		  List JSON → JSON
	  , bool :
		  Bool → JSON
	  , null :
		  JSON
	  , number :
		  Double → JSON
	  , object :
		  List { mapKey : Text, mapValue : JSON } → JSON
	  , string :
		  Text → JSON
	  }
	)
→ json.object
  [ { mapKey = "foo", mapValue = json.null }
  , { mapKey =
		"bar"
	, mapValue =
		json.array [ json.number 1.0, json.bool True ]
	}
  ]
