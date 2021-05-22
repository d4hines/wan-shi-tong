open Migrate_parsetree

let pp_boiler _ _ = ()

let pp_ta_boiler _ _ _ = ()

let to_yojson_boiler _ = `String ""

let to_yojson_ta_boiler _ _ = `String ""

module Stdlib = struct
  include Stdlib

  let ref_to_yojson to_yojson ref = to_yojson !ref
end

module Location = struct
  include Location

  let pp _ _ = ()

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Ident = struct
  include Ident

  module Set = struct
    include Set

    let pp = Ident.Set.print

    let to_yojson t =
      `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
  end

  let pp = Ident.print

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Path = struct
  include Path

  type t = [%import: Path.t] [@@deriving show { with_path = false }, to_yojson]
end

module Env = struct
  include Env

  let pp formatter _env =
    Format.fprintf formatter ("env" [@reason.raw_literal "env"])

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Longident = struct
  include Longident

  type t = [%import: Longident.t]
  [@@deriving show { with_path = false }, to_yojson]
end

module Typedtree = struct
  include Typedtree

  type partial = [%import: Typedtree.partial]
  [@@deriving show { with_path = false }, to_yojson]

  
end
