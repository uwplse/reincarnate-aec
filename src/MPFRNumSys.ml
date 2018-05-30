(** MPFR Number Systems *)

open Util
open NumSys

(** MPFR arbitrary precision number systems *)

module type MPFR_CONFIG = sig
  val prec  : int
  val rmode : Mpfr.round
end

module MakeMPFRNumCore (C : MPFR_CONFIG) : NUM_CORE = struct
  include C

  type t = Mpfrf.t

  let of_string s =
    let x = Mpfr.init2 prec in
    Mpfr.set_str x s ~base:10 rmode;
    Mpfrf.of_mpfr x

  let pi =
    let x = Mpfr.init2 prec in
    ignore (Mpfr.const_pi x rmode);
    Mpfrf.of_mpfr x

  let lift_uop op a =
    let x = Mpfr.init2 prec in
    ignore (op x (Mpfrf.to_mpfr a) rmode);
    Mpfrf.of_mpfr x

  let lift_bop op a b =
    let x = Mpfr.init2 prec in
    ignore (op x (Mpfrf.to_mpfr a) (Mpfrf.to_mpfr b) rmode);
    Mpfrf.of_mpfr x

  let neg a   = lift_uop Mpfr.neg a
  let abs a   = lift_uop Mpfr.abs a
  let add a b = lift_bop Mpfr.add a b
  let sub a b = lift_bop Mpfr.sub a b
  let mul a b = lift_bop Mpfr.mul a b
  let div a b = lift_bop Mpfr.div a b
  let rem a b = lift_bop Mpfr.remainder a b
  let sqrt a  = lift_uop Mpfr.sqrt a

  let sin a     = lift_uop Mpfr.sin a
  let cos a     = lift_uop Mpfr.cos a
  let tan a     = lift_uop Mpfr.tan a
  let asin a    = lift_uop Mpfr.asin a
  let acos a    = lift_uop Mpfr.acos a
  let atan2 a b = lift_bop Mpfr.atan2 a b

  let order =
    cmp_of_cmpi Mpfrf.cmp

  let __oprec      = ref 10
  let get_oprec () = !__oprec
  let set_oprec p  = __oprec := p
  let to_string x =
    let x = Mpfrf.to_mpfr x in
    (* the "... + 1" on the next line below is total bogus, but takes care of
       the fact that float interprets precision to be "digits after decimal
       point" while mpfr interprets it as "significant digits".

       you might say: wait, but how does adding one fix that?

       you would be right, except that for the only nontrivial top-level
       constant (pi), adding one is exactly the right thing to do, since
       pi has one digit in front of the decimal point
     *)
    let (s,e) = Mpfr.get_str ~base:10 ~digits:(get_oprec () + 1) x rmode in
    if not (Mpfr.number_p x) then s
    else if s = "" then "0."
    else if s.[0] = '-'
    then Format.sprintf "-0.%sE%d" String.(sub s 1 (length s - 1)) e
    else Format.sprintf "0.%sE%d" s e

  (* here's an alternative implementation that gives full precision: *)
    (* Mpfrf.to_string x *)

  let forget x = x
  let get_stats () = 0
  let print_table () = ()
end

module MakeMPFRNum (C : MPFR_CONFIG) : NUM =
  MakeNumSys(MakeMPFRNumCore(C))

module MPFRNum_128 = MakeMPFRNum(struct
  let prec  = 128
  let rmode = Mpfr.Near
end)

module MPFRNum_256 = MakeMPFRNum(struct
  let prec  = 256
  let rmode = Mpfr.Near
end)

module MPFRNum_512 = MakeMPFRNum(struct
  let prec  = 512
  let rmode = Mpfr.Near
end)

module MPFRNum_1024 = MakeMPFRNum(struct
  let prec  = 1024
  let rmode = Mpfr.Near
end)

module MPFRNum_2048 = MakeMPFRNum(struct
  let prec  = 2048
  let rmode = Mpfr.Near
end)

module MPFRNum_30000 = MakeMPFRNum(struct
  let prec  = 30000
  let rmode = Mpfr.Near
end)

(** Debugging number system *)

module DebugNumCore = struct
  (* use this FC to also catch nan, inf, subnormal, etc.
  module FC = OrdinaryFloatNumCore
  *)
  module FC = FloatNumCore
  module MC = MakeMPFRNumCore(struct
                let prec  = 256
                let rmode = Mpfr.Near
              end)
  module SC = SymNumCore

  type t = FC.t * MC.t * SC.t

  let get_oprec () =
    let fp = FC.get_oprec () in
    let mp = MC.get_oprec () in
    assert (fp = mp);
    fp

  let set_oprec p = begin
    FC.set_oprec p;
    MC.set_oprec p;
  end

  let to_string (f, m, s) =
    Printf.sprintf "<| %s || %s || %s |>"
      (FC.to_string f)
      (MC.to_string m)
      (SC.to_string s)

  let of_string x =
    ( FC.of_string x
    , MC.of_string x
    , SC.of_string x )

  let pi =
    ( FC.pi
    , MC.pi
    , SC.pi )

  let neg (f, m, s) =
    ( FC.neg f
    , MC.neg m
    , SC.neg s )

  let abs (f, m, s) =
    ( FC.abs f
    , MC.abs m
    , SC.abs s )

  let add (fa, ma, sa) (fb, mb, sb) =
    ( FC.add fa fb
    , MC.add ma mb
    , SC.add sa sb )

  let sub (fa, ma, sa) (fb, mb, sb) =
    ( FC.sub fa fb
    , MC.sub ma mb
    , SC.sub sa sb )

  let mul (fa, ma, sa) (fb, mb, sb) =
    ( FC.mul fa fb
    , MC.mul ma mb
    , SC.mul sa sb )

  let div (fa, ma, sa) (fb, mb, sb) =
    ( FC.div fa fb
    , MC.div ma mb
    , SC.div sa sb )

  let rem (fa, ma, sa) (fb, mb, sb) =
    ( FC.rem fa fb
    , MC.rem ma mb
    , SC.rem sa sb )

  let sqrt (f, m, s) =
    ( FC.sqrt f
    , MC.sqrt m
    , SC.sqrt s )

  let sin (f, m, s) =
    ( FC.sin f
    , MC.sin m
    , SC.sin s )

  let cos (f, m, s) =
    ( FC.cos f
    , MC.cos m
    , SC.cos s )

  let tan (f, m, s) =
    ( FC.tan f
    , MC.tan m
    , SC.tan s )

  let acos (f, m, s) =
    ( FC.acos f
    , MC.acos m
    , SC.acos s )

  let asin (f, m, s) =
    ( FC.asin f
    , MC.asin m
    , SC.asin s )

  let atan2 (fa, ma, sa) (fb, mb, sb) =
    ( FC.atan2 fa fb
    , MC.atan2 ma mb
    , SC.atan2 sa sb )

  let order (fa, ma, sa) (fb, mb, sb) =
    let fo = FC.order fa fb in
    let mo = MC.order ma mb in
    if fo = mo then
      fo
    else begin
      set_oprec 30;
      let msg =
        String.concat "\n"
          [ Printf.sprintf "DebugNum: bad compare\n  a: %s\n  b: %s"
              (to_string (fa, ma, sa))
              (to_string (fb, mb, sb))
          ; Printf.sprintf "FL = %s / MPFR = %s"
              (string_of_cmp fo)
              (string_of_cmp mo)
          ]
      in
      failwith msg
  end

  let forget x = x
  let get_stats () = 0
  let print_table () = ()
end

module DebugNum : NUM =
  MakeNumSys(MakeCheckedNumCore(DebugNumCore)(struct
    module DNC = DebugNumCore
    module FC  = DNC.FC
    module MC  = DNC.MC
    module SC  = DNC.SC

    type t = DNC.t

    (* These are only used for [check] below, not for client [equiv] calls.
     * That code is provided by the MakeNumSys functor.
     *)

    let __eps_abs = ref (FC.of_string "1e-10")
    let __eps_rel = ref (FC.of_string "1e-10")

    let equiv a b =
      let diff = FC.abs (FC.sub a b) in
      if FC.order diff !__eps_abs <> GT then
        (* needed when comparing near 0 *)
        true
      else
        let maxAB = max FC.order (FC.abs a) (FC.abs b) in
        FC.order diff (FC.mul maxAB !__eps_rel) <> GT

    let check (f, m, s) =
      (* perhaps can make this conversion faster, but types hidden *)
      (* would rather not change the strict interfaces for now     *)
      let mf = FC.of_string (MC.to_string m) in
      if equiv f mf then
        (f, m, s)
      else
        failwith (Printf.sprintf
          "DebugNum: error too high: %s\n"
            (DNC.to_string (f, m, s)))
  end))

