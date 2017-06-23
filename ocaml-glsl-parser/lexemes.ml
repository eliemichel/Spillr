open Parser

let types = [
    "void";
    "bool";
    "int";
    "uint";
    "float";
    "double";
    "vec2";
    "vec3";
    "vec4";
    "dvec2";
    "dvec3";
    "dvec4";
    "bvec2";
    "bvec3";
    "bvec4";
    "ivec2";
    "ivec3";
    "ivec4";
    "uvec2";
    "uvec3";
    "uvec4";
    "mat2";
    "mat3";
    "mat4";
    "mat2x2";
    "mat2x3";
    "mat2x4";
    "mat3x2";
    "mat3x3";
    "mat3x4";
    "mat4x2";
    "mat4x3";
    "mat4x4";
    "dmat2";
    "dmat3";
    "dmat4";
    "dmat2x2";
    "dmat2x3";
    "dmat2x4";
    "dmat3x2";
    "dmat3x3";
    "dmat3x4";
    "dmat4x2";
    "dmat4x3";
    "dmat4x4";
    "sampler1D";
    "image1D";
    "sampler2D";
    "image2D";
    "sampler3D";
    "image3D";
    "samplerCube";
    "imageCube";
    "sampler2DRect";
    "image2DRect";
    "sampler1DArray";
    "image1DArray";
    "sampler2DArray";
    "image2DArray";
    "samplerBuffer";
    "imageBuffer";
    "sampler2DMS";
    "image2DMS";
    "sampler2DMSArray";
    "image2DMSArray";
    "samplerCubeArray";
    "imageCubeArray";
    "sampler1DShadow";
    "sampler2DShadow";
    "sampler2DRectShadow";
    "sampler1DArrayShadow";
    "sampler2DArrayShadow";
    "samplerCubeShadow";
    "samplerCubeArrayShadow";
    "isampler1D";
    "iimage1D";
    "isampler2D";
    "iimage2D";
    "isampler3D";
    "iimage3D";
    "isamplerCube";
    "iimageCube";
    "isampler2DRect";
    "iimage2DRect";
    "isampler1DArray";
    "iimage1DArray";
    "isampler2DArray";
    "iimage2DArray";
    "isamplerBuffer";
    "iimageBuffer";
    "isampler2DMS";
    "iimage2DMS";
    "isampler2DMSArray";
    "iimage2DMSArray";
    "isamplerCubeArray";
    "iimageCubeArray";
    "atomic_uint";
    "usampler1D";
    "uimage1D";
    "usampler2D";
    "uimage2D";
    "usampler3D";
    "uimage3D";
    "usamplerCube";
    "uimageCube";
    "usampler2DRect";
    "uimage2DRect";
    "usampler1DArray";
    "uimage1DArray";
    "usampler2DArray";
    "uimage2DArray";
    "usamplerBuffer";
    "uimageBuffer";
    "usampler2DMS";
    "uimage2DMS";
    "usampler2DMSArray";
    "uimage2DMSArray";
    "usamplerCubeArray";
    "uimageCubeArray"
]

let keywords_assoc = [
    "struct",        STRUCT;
    "else",          ELSE;
    "for",           FOR;
    "if",            IF;
    "return",        RETURN;
    "discard",       DISCARD;
    "while",         WHILE;
    "attribute",     ATTRIBUTE;
    "const",         CONST;
    "uniform",       UNIFORM;
    "varying",       VARYING;
    "buffer",        BUFFER;
    "shared",        SHARED;
    "coherent",      COHERENT;
    "volatile",      VOLATILE;
    "restrict",      RESTRICT;
    "readonly",      READONLY;
    "writeonly",     WRITEONLY;
    "atomic_uint",   ATOMIC_UINT;
    "layout",        LAYOUT;
    "centroid",      CENTROID;
    "flat",          FLAT;
    "smooth",        SMOOTH;
    "noperspective", NOPERSPECTIVE;
    "patch",         PATCH;
    "sample",        SAMPLE;
    "break",         BREAK;
    "continue",      CONTINUE;
    "do",            DO;
    "switch",        SWITCH;
    "case",          CASE;
    "default",       DEFAULT;
    "subroutine",    SUBROUTINE;
    "in",            IN;
    "out",           OUT;
    "inout",         INOUT;
    "invariant",     INVARIANT;
    "precise",       PRECISE;
    "precision",     PRECISION;
    "lowp",          LOWP;
    "mediump",       MEDIUMP;
    "highp",         HIGHP;
]

let id_or_keyword =
    let keywords_h = Hashtbl.create 97 in
    let types_h = Hashtbl.create 97 in
        List.iter (fun (s, t) -> Hashtbl.add keywords_h s t) keywords_assoc;
        List.iter (fun s -> Hashtbl.add types_h s ()) types;
        fun s ->
            try
                Hashtbl.find keywords_h s
            with Not_found -> (
                if Hashtbl.mem types_h s
                then TIDENT s
                else IDENT s
            )
