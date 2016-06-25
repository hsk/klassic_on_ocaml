open AstNode

type value =
  | BoxedByte of byte
  | BoxedShort of short
  | BoxedInt of int
  | BoxedLong of long
  | BoxedBoolean of bool
  | BoxedDouble of double
  | BoxedFloat of float
  | FunctionValue of functionLiteral * environment option
  | NativeFunctionValue of (value list -> value)
  | UnitValue
(*| ObjectValue of anyRef*)
  | ListValue of value list
and environment = Environment of (string * value) list ref * environment option

let rec show = function
  | BoxedByte(byte) -> string_of_byte byte
  | BoxedShort(short) -> string_of_short short
  | BoxedInt(int) -> string_of_int int
  | BoxedLong(long) -> string_of_long long
  | BoxedBoolean(bool) -> string_of_bool bool
  | BoxedDouble(double) -> string_of_double double
  | BoxedFloat(float) -> string_of_float float
  | FunctionValue(value, environment) ->
    Printf.sprintf "function value:%s environment:%s"
      (show_functionLiteral value)
      (show_env_opt environment)
  | NativeFunctionValue(body) -> "native function"
  | UnitValue -> "()"
(*| ObjectValue(value) -> value.toString*)
  | ListValue(vs) -> "[" ^ String.concat ", " (List.map show vs) ^ "]"
and show_env = function
  | Environment(vs, parent) ->
    Printf.sprintf "Environment(Map(%s))"
      (String.concat "," (List.map (fun (k,v)-> k ^ " -> " ^ show v) !vs))
and show_env_opt = function
  | None -> "None"
  | Some(env) -> Printf.sprintf "Some(%s)" (show_env env)

(*
def classOfValue(value: Value): java.lang.Class[_]= match value with
  case BoxedBoolean(v) -> classOf[Boolean]
  case BoxedByte(v) -> classOf[Byte]
  case BoxedShort(v) -> classOf[Short]
  case BoxedInt(v) -> classOf[Int]
  case BoxedLong(v) -> classOf[Long]
  case BoxedFloat(v) -> classOf[Float]
  case BoxedDouble(v) -> classOf[Double]
  case ObjectValue(v) -> v.getClass
  case otherwise -> otherwise.getClass
}

def boxedClassOfValue(value: Value): java.lang.Class[_]= match value with
  case BoxedBoolean(v) -> classOf[java.lang.Boolean]
  case BoxedByte(v) -> classOf[java.lang.Byte]
  case BoxedShort(v) -> classOf[java.lang.Short]
  case BoxedInt(v) -> classOf[java.lang.Integer]
  case BoxedLong(v) -> classOf[java.lang.Long]
  case BoxedFloat(v) -> classOf[java.lang.Float]
  case BoxedDouble(v) -> classOf[java.lang.Double]
  case ObjectValue(v) -> v.getClass
  case otherwise -> otherwise.getClass
}

def boxedClassesOfValues(values: Array[Value]): Array[java.lang.Class[_]] = values.map(boxedClassOfValue)

def classesOfValues(values: Array[Value]):  Array[java.lang.Class[_]] = values.map(classOfValue)

def fromKlassic(value: Value): AnyRef = match value with
  case BoxedBoolean(v) -> new java.lang.Boolean(v)
  case BoxedByte(v) -> new java.lang.Byte(v)
  case BoxedShort(v) -> new java.lang.Short(v)
  case BoxedInt(v) -> new java.lang.Integer(v)
  case BoxedLong(v) -> new java.lang.Long(v)
  case BoxedFloat(v) -> new java.lang.Float(v)
  case BoxedDouble(v) -> new java.lang.Double(v)
  case ObjectValue(v) -> v
  case UnitValue -> UnitValue
  case otherwise -> otherwise


def toKlassic(value: AnyRef): Value = match value with
  case v:java.lang.Boolean -> BoxedBoolean(v.booleanValue())
  case v:java.lang.Byte -> BoxedByte(v.byteValue())
  case v:java.lang.Short -> BoxedShort(v.shortValue())
  case v:java.lang.Integer -> BoxedInt(v.intValue())
  case v:java.lang.Long -> BoxedLong(v.intValue())
  case v:java.lang.Float -> BoxedFloat(v.floatValue())
  case v:java.lang.Double -> BoxedDouble(v.doubleValue())
  case UnitValue -> UnitValue
  case otherwise -> ObjectValue(otherwise)
*)
