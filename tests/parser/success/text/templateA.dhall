    \(record : { name        : Text
               , value       : Double
               , taxed_value : Double
               , in_ca       : Bool
               }
     ) ->  ''
Hello ${record.name}
You have just won ${Double/show record.value} dollars!
${ if record.in_ca
   then "Well, ${Double/show record.taxed_value} dollars, after taxes"
   else ""
 }
''
