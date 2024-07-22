

- Primitives
    - char, bool, u8, i32, f32
    - constant arrays, list, slice
- Product types
    - records (struct)
    - tuples
- `Ptr a` - Pointer to value of type `a`
- `Ref a` - Boxed mutable value of type `a` (this can promote to reference counted / garbage collected)
- linear types (must be used once)
- typeclasses

```rust

// records

type Slice t = {
    ptr: Ptr t,
    len: i32
}

type DArray t = {
    capacity: i32,
    len: i32,
    data: &[t] // slice of type t - syntactic sugar for 'Slice t'
}
// TODO: what would struct functions look like?
impl DArray t {
}

@repr(std140) // use glsl's std140 memory alignment layout
type CameraUniforms = {
    view_projection: Mat4,
    view_direction: Vec3
}
@deriving(Show, Codec, Bindable)

// show = Display
// codec = Serialize + Deserialize
// bindable = shader binding description (shown below)

module ShaderBindings {
    type Binding =  
        | Bytes of i32 // size
        | Texture of Handle
        | Buffer of i32 * Handle

    type ShaderLayout = StackArray Binding 8

    trait Bindable {
        type T
        val layout : T -> ShaderLayout      // knows how to describe the needed bindings
        val bind : CmdEncoder -> T -> unit  // knows how to bind shader data 
    }
}

trait Alloc {
    // alloc is parametrically polymorphic but type 'a' must implement Size
    val alloc : a -> Ptr a
        where a impl Size
        
    // default implementation of the trait
    let alloc a =
        open Libc in
        malloc a.size

    val free : Ptr a -> unit
    val safe_free : Ptr a -> result (unit, MemErr)
}

// ADTs
type TrafficLight = Red | Amber | Green

type HashMap key value = {
    // ....
}

type Graph a = HashMap a (List a) where a implements Eq

```


-----

(* let ch = open_out "output.js" in *)
    (* Printf.fprintf ch "%s" asm; flush ch; close_out ch;  *)
    (* write assembly to file *) 
    (* let _ = print_string "assembler"; Sys.command "nasm -f elf64 output.s -o output.o" in
    let _ = print_string " -> linker"; Sys.command "gcc -no-pie -nostartfiles output.o -o output.exe" in
    print_string " -> executable!\n"; flush stdout; let _ =  Sys.command "node ./output.js" in *)
