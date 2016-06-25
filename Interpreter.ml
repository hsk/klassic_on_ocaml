open AstNode

let counter: int ref = ref 0
let symbol (): string =
  let name = "var" ^ string_of_int counter in
  counter := !counter + 1;
  name

let reportError (message: string) = 
  raise (InterpreterException message)

(*
let findMethod(self: anyRef, name: string, params: value array): methodSearchResult = 
  val selfClass = self.getClass
  val nameMatchedMethods = selfClass.getMethods.filter {
    _.getName = name
  }
  nameMatchedMethods.find { m =>
    val parameterCountMatches = m.getParameterCount = params.length
    val parameterTypes = Value.classesOfValues(params)
    val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall{ | (arg, param) =>
      arg.isAssignableFrom(param)
    }
    parameterCountMatches && parameterTypesMatches
  }.map{m =>
    m.setAccessible(true)
    UnboxedVersionMethodFound(m)
  }.orElse({
    nameMatchedMethods.find{m =>
      val parameterCountMatches = m.getParameterCount = params.length
      val boxedParameterTypes = Value.boxedClassesOfValues(params)
      val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall{ | (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && boxedParameterTypesMatches
    }
  }.map{m =>
    m.setAccessible(true)
    BoxedVersionMethodFound(m)
  }).getOrElse(NoMethodFound)


let findConstructor(target: class[_], params: Array[value]): constructorSearchResult = 
  let constructors = target.getConstructors in
  constructors.find{c =>
    val parameterCountMatches = c.getParameterCount = params.length
    val unboxedParameterTypes = Value.classesOfValues(params)
    val parameterTypesMatches  = (c.getParameterTypes zip unboxedParameterTypes).forall{ | (arg, param) =>
      arg.isAssignableFrom(param)
    }
    parameterCountMatches && parameterTypesMatches
  }.map{c =>
    UnboxedVersionConstructorFound(c)
  }.orElse({
    constructors.find{c =>
      val parameterCountMatches = c.getParameterCount = params.length
      val boxedParameterTypes = Value.boxedClassesOfValues(params)
      val parameterTypesMatches  = (c.getParameterTypes zip boxedParameterTypes).forall{ | (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }
  }.map { c =>
    BoxedVersionConstructorFound(c)
  }).getOrElse(NoConstructorFound)
*)

let rec evaluate(env: environment, node: astNode): value = 
  let rec rewrite(node: astNode): astNode = match node with
    | Block(location, expressions) -> Block(location, List.map rewrite expressions)
    | IfExpression(location, cond, pos, neg) ->
      IfExpression(location, rewrite(cond), rewrite(pos), rewrite(neg))
    | WhileExpression(location, condition, body) ->
      WhileExpression(location, rewrite(condition), rewrite(body))
    | ForeachExpression(location, name, collection, body) as e ->
      let itVariable = symbol() in
      let location = AstNode.location e in
      Block(location, [
        ValDeclaration(location, itVariable, None, MethodCall(location, rewrite(collection), "iterator", []));
        WhileExpression(
          location,
          BinaryExpression(
            location,
            Operator.EQUAL,
            MethodCall(location, Identifier(location, itVariable), "hasNext", []),
            BooleanNode(location, true)
          ),
          Block(location, [
            ValDeclaration(location, name, None, MethodCall(location, Identifier(location, itVariable), "next", []));
            body
          ])
        )
      ])
    | BinaryExpression(location, operator, lhs, rhs) ->
      BinaryExpression(location, operator, rewrite(lhs), rewrite(rhs))
    | MinusOp(location, operand) -> MinusOp(location, rewrite(operand))
    | PlusOp(location, operand) -> PlusOp(location, rewrite(operand))
    | StringNode(location, value) as iteral -> literal
    | IntNode(location, value) as iteral -> literal
    | LongNode(location, value) as iteral -> literal
    | ShortNode(location, value) as iteral -> literal
    | ByteNode(location, value) as iteral -> literal
    | BooleanNode(location, value) as iteral -> literal
    | DoubleNode(location, value) as iteral -> literal
    | FloatNode(lcation, value) as iteral -> literal
    | Identifier(_, name) as node -> node
    | Assignment(location, variable, value) -> Assignment(location, variable, rewrite(value))
    | ValDeclaration(location, variable, optionalType, value) -> ValDeclaration(location, variable, optionalType, rewrite(value))
    | FunctionLiteral(location, params, proc) -> FunctionLiteral(location, params, rewrite(proc))
    | FunctionDefinition(location, name, func) ->
      begin match rewrite(FunctionLiteral(location, name, func)) with
      | FunctionLiteral(location, params, proc) -> FunctionDefinition(location, name, literal)
      | _ -> assert false
      end
    | FunctionCall(location, func, params) -> FunctionCall(location, rewrite(func), List.map rewrite params)
    | ListLiteral(location, elements) ->  ListLiteral(location, List.map rewrite elements)
    | NewObject(location, className, params) -> NewObject(location, className, List.map rewrite params)
    | MethodCall(location ,self, name, params) -> MethodCall(location, rewrite(self), name, List.map rewrite params)
  in
  let rec evalRecursive(node: astNode): value =
    match node with
      | Block(location, exprs) ->
        let local = Environment(Some(env)) in
        List.fold_left (fun (result, x) -> evaluate(local, x)) UnitValue exprs
      | WhileExpression(location, cond, body) ->
        while evalRecursive(cond) = BoxedBoolean(true) do
          evalRecursive(body)
        done;
        UnitValue
      | IfExpression(location, condition, pos, neg) ->
        begin match evalRecursive(condition) with
          | BoxedBoolean(true) -> evalRecursive(pos)
          | BoxedBoolean(false) -> evalRecursive(neg)
          | _ -> reportError("type error")
        end
      | BinaryExpression(location, Operator.AND2, lhs, rhs) ->
        begin evalRecursive(lhs) with
          | BoxedBoolean(true) -> evalRecursive(rhs)
          | BoxedBoolean(false) -> BoxedBoolean(false)
          | _ -> reportError("type error")
        end
      | BinaryExpression(location, Operator.BAR2, lhs, rhs) ->
        begin evalRecursive(lhs) with
          | BoxedBoolean(false) -> evalRecursive(rhs)
          | BoxedBoolean(true) -> BoxedBoolean(true)
          | _ -> reportError("type error")
        end
      | BinaryExpression(location, Operator.EQUAL, left, right) ->
        begin (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedBoolean(lval = rval)
          | (BoxedBoolean(lval), BoxedBoolean(rval)) -> BoxedBoolean(lval = rval)
          (* | (BoxedBoolean(lval), ObjectValue(rval:java.lang.Boolean)) -> BoxedBoolean(lval = rval.booleanValue())
          | (ObjectValue(lval:java.lang.Boolean), BoxedBoolean(rval)) -> BoxedBoolean(lval.booleanValue() = rval)
          | (ObjectValue(lval), ObjectValue(rval)) -> BoxedBoolean(lval = rval) *)
          | _ -> reportError("comparation must be done between same types")
        end
      | BinaryExpression(location, Operator.LESS_THAN, left, right) ->
        begin (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedBoolean(lval < rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedBoolean(lval < rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedBoolean(lval < rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedBoolean(lval < rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedBoolean(lval < rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedBoolean(lval < rval)
          | _ -> reportError("comparation must be done between numeric types")
        end
      | BinaryExpression(location, Operator.GREATER_THAN, left, right) ->
        begin (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedBoolean(lval > rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedBoolean(lval > rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedBoolean(lval > rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedBoolean(lval > rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedBoolean(lval > rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedBoolean(lval > rval)
          | _ -> reportError("comparation must be done between numeric types")
        end
      | BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) ->
        begin (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedBoolean(lval <= rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedBoolean(lval <= rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedBoolean(lval <= rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedBoolean(lval <= rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedBoolean(lval <= rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedBoolean(lval <= rval)
          | _ -> reportError("comparation must be done between numeric types")
        end
      | BinaryExpression(location, Operator.GREATER_EQUAL, left, right) ->
        begin (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedBoolean(lval >= rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedBoolean(lval >= rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedBoolean(lval >= rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedBoolean(lval >= rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedBoolean(lval >= rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedBoolean(lval >= rval)
          | _ -> reportError("comparation must be done between numeric types")
        end
      | BinaryExpression(location, Operator.ADD, left, right) ->
        begin match (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedInt(lval + rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedLong(lval + rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedShort(lval + rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedByte(lval + rval)
          (* | (ObjectValue(lval:String), rval) -> ObjectValue(lval + rval)
          | (lval, ObjectValue(rval:String)) -> ObjectValue(lval + rval) *)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedFloat((lval +. rval))
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedDouble(lval +. rval)
          | _ -> reportError("arithmetic operation must be done between the same numeric types")
        end
      | BinaryExpression(location, Operator.SUBTRACT, left, right) ->
        begin match (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedInt(lval - rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedLong(lval - rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedShort(lval - rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedByte(lval - rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedFloat(lval -. rval)
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedDouble(lval -. rval)
          | _ -> reportError("arithmetic operation must be done between the same numeric types")
        end
      | BinaryExpression(location, Operator.MULTIPLY, left, right) ->
        begin match (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedInt(lval * rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedLong(lval * rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedShort(lval * rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedByte(lval * rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedFloat((lval *. rval))
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedDouble(lval *. rval)
          | _ -> reportError("arithmetic operation must be done between the same numeric types")
        end
      | BinaryExpression(location, Operator.DIVIDE, left, right) ->
        begin match (evalRecursive(left), evalRecursive(right)) with
          | (BoxedInt(lval), BoxedInt(rval)) -> BoxedInt(lval / rval)
          | (BoxedLong(lval), BoxedLong(rval)) -> BoxedLong(lval / rval)
          | (BoxedShort(lval), BoxedShort(rval)) -> BoxedShort(lval / rval)
          | (BoxedByte(lval), BoxedByte(rval)) -> BoxedByte(lval / rval)
          | (BoxedFloat(lval), BoxedFloat(rval)) -> BoxedFloat((lval /. rval))
          | (BoxedDouble(lval), BoxedDouble(rval)) -> BoxedDouble(lval /. rval)
          | _ -> reportError("arithmetic operation must be done between the same numeric types")
        end
      | MinusOp(location, operand) ->
        begin match evalRecursive(operand) with
          | BoxedInt(value) -> BoxedInt(-value)
          | BoxedLong(value) -> BoxedLong(-value)
          | BoxedShort(value) -> BoxedShort(-value)
          | BoxedByte(value) -> BoxedByte((-value))
          | BoxedFloat(value) -> BoxedFloat(-value)
          | BoxedDouble(value) -> BoxedDouble(-value)
          | _ -> reportError("- cannot be applied to non-integer value")
        end
      | PlusOp(location, operand) ->
        begin match evalRecursive(operand) with
          | BoxedInt(value) -> BoxedInt(value)
          | BoxedLong(value) -> BoxedLong(value)
          | BoxedShort(value) -> BoxedShort(value)
          | BoxedByte(value) -> BoxedByte(value)
          | BoxedFloat(value) -> BoxedFloat(value)
          | BoxedDouble(value) -> BoxedDouble(value)
          | _ -> reportError("+ cannot be applied to non-integer value")
        end
      | IntNode(location, value) ->
        BoxedInt(value)
      | StringNode(location, value) ->
        ObjectValue(value)
      | LongNode(location, value) ->
        BoxedLong(value)
      | ShortNode(location, value) ->
        BoxedShort(value)
      | ByteNode(location, value) ->
        BoxedByte(value)
      | DoubleNode(location, value) ->
        BoxedDouble(value)
      | FloatNode(location, value) ->
        BoxedFloat(value)
      | BooleanNode(location, value) ->
        BoxedBoolean(value)
      | ListLiteral(location, elements) ->
        let params = List.map(fun e -> Value.fromKlassic(evalRecursive(e)) elements in
        let newList = new java.util.ArrayList[Any] in
        params.foreach{param ->
          newList.add(param)
        };
        ObjectValue(newList)
      | Identifier(location, name) -> env(name)
      | ValDeclaration(location, vr, optVariableType, value) ->
        env(vr) = evalRecursive(value)
      | Assignment(location, vr, value) ->
        env.set(vr, evalRecursive(value))
      | FunctionLiteral(location, _, _) as literal ->
        FunctionValue(literal, Some(env))
      | FunctionDefinition(location, name, func) ->
        env(name) = FunctionValue(func, Some(env)): Value
      | MethodCall(location, self, name, params) ->
        begin match evalRecursive(self) with 
          | ObjectValue(value) ->
            let paramsArray = Array.of_list(List.map(fun p -> evalRecursive(p)) params) in
            begin match findMethod(value, name, paramsArray) match {
              | UnboxedVersionMethodFound(method) ->
                let actualParams = Array.map Value.fromKlassic paramsArray in
                Value.toKlassic(method.invoke(value, actualParams:_*))
              | BoxedVersionMethodFound(method) ->
                let actualParams = Array.map Value.fromKlassic paramsArray in
                Value.toKlassic(method.invoke(value, actualParams:_*))
              | NoMethodFound ->
                raise (IllegalArgumentException(Printf.sprintf "%s.%s(%s)" self name params))
            end
          | otherwise ->
            failwith("cannot reach here: "^Value.show otherwise)
        end
      | NewObject(location, className, params) ->
        let paramsArray = params.map{evalRecursive}.toArray in
        begin match findConstructor(Class.forName(className), paramsArray) match {
          | UnboxedVersionConstructorFound(constructor) ->
            val actualParams = paramsArray.map{Value.fromKlassic}
            Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
          | BoxedVersionConstructorFound(constructor) ->
            val actualParams = paramsArray.map{Value.fromKlassic}
            Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
          | NoConstructorFound ->
            raise (IllegalArgumentException(Printf.sprintf "newObject(%s, %s)" className params))
        end
      | FunctionCall(location, func, params) ->
        begin match evalRecursive(func) with
          | FunctionValue(FunctionLiteral(location, fparams, proc), cenv) ->
            let local = Environment(cenv) in
            List.iter (fun (fp, ap) ->
              local(fp.name) = evalRecursive(ap)
            ) (List.combine fparams params)
            evaluate(local, proc)
          | NativeFunctionValue(body) ->
            let actualParams = List.map evalRecursive params in
            if body.isDefinedAt(actualParams) then 
              body(List.map evalRecursive params)
            else
              reportError("parameters are not matched to the function's arguments")
            
          | _ ->
            reportError("unknown error")
        end
      | ForeachExpression(location, _, _, _) as oterwise -> sys.error("cannot reach here: "^otherwise)
    }
  in
  evalRecursive(rewrite(node))

let evaluateNode(node: astNode): value = evaluate(builtinEnvironment, node)

let evaluateString(program: string, fileName: string = "<no file>"): value =
  parser.parse(program) match {
    | parser.Success(node: AstNode, _) => evaluateNode(node)
    | parser.Failure(m, n) => reportError (n.pos ^ ":" ^ m)
    | parser.Error(m, n) => reportError (n.pos ^ ":" ^ m)
  }

let evaluateFile(file: File): Value = using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))) in =>
  let program = Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString in
  evaluateString(program)

let parse(program: string): astNode =
  parser.parse(program) match {
    | parser.Success(node: AstNode, _) => node
    | parser.Failure(m, n) => reportError (n.pos ^ ":" ^ m)
    | parser.Error(m, n) => reportError (n.pos ^ ":" ^ m)
  }

let builtinEnvironment = Environment([], None)
