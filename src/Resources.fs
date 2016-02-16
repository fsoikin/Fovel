module private Fovel.Resources

type private Dummy = Dummy
let private resman = lazy System.Resources.ResourceManager("Resources", typeof<Dummy>.Assembly)

let CoreLib() = resman.Value.GetString "CoreLib.FSharp"