                              CL-HORDE3D
                              ==========

Author: Ole Arndt <ole@sugarshark.com>
Date: 2009-12-04 02:19:46 CET


Table of Contents
=================
What is Horde3D? 
What is CL-HORDE3D? 
Dependencies 
    Lisp Libraries 
    Foreign libraries 
Compatibility with different Horde3D versions 
Compatibility with Lisps 
Translating the Horde3D API from C to CL 
Running the examples 


What is Horde3D? 
~~~~~~~~~~~~~~~~~

  From the Horde3D website [http://horde3d.org]:

ORG-BLOCKQUOTE-START
Horde3D is a small open source 3D rendering engine. It is written in
an effort to create a graphics engine that offers the stunning
visual effects expected in next-generation games while at the same
time being as lightweight and conceptually clean as
possible. Horde3D has a simple and intuitive interface accessible
from virtually any programming language and is particularly suitable
for rendering large crowds of animated characters in next-generation
quality.
ORG-BLOCKQUOTE-END

  Horde3D is an OpenGL based graphics engine that does not use the
  fixed function pipeline but is built around the usage of shader
  programs. It only runs on OpenGL 2.0 compatible graphics
  hardware. It is a rather lean engine. Though it is written in C++,
  it exports a C API which can be easily wrapped with 

What is CL-HORDE3D? 
~~~~~~~~~~~~~~~~~~~~

  A CFFI wrapper for the Horde3D library. Flashy 3D graphics with
  Lisp.

Dependencies 
~~~~~~~~~~~~~

Lisp Libraries 
===============
   - CFFI
   - Lispbuilder-sdl (for the examples)

Foreign libraries 
==================
   - Horde3D
   - SDL (for the examples)
   

Compatibility with different Horde3D versions 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cl-horde3d currently works together with the
  Horde3D_SDK_1.0.0-beta4. It should also run with the community svn
  version, but last time I tried the Chicago Example failed.

  There is also a horde3d-beta3-branch in the GIT repository which
  contains an older version of cl-horde3d which should run with
  Horde3D 1.0.0 beta3.
  

Compatibility with Lisps 
~~~~~~~~~~~~~~~~~~~~~~~~~

  CL-HORDE3D should theoretically run on all lisps that CFFI supports.
  I tested cl-horde3d with the following lisps on a amd64 Gentoo
  Linux system:

  Clozure Common Lisp: Works with 1.3 and 1.4.
  Clisp: Works with 2.48
  SBCL: Crashes in the foreign code. I suspect this has something
            to do with SBCLs memory model.
  
  I would like to hear about success or failure with other lisps.

Translating the Horde3D API from C to CL 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  I chose to not translate the API literally, but to adjust the
  naming to the habits in the Common Lisp world. That's what I did:

  - I dropped the h3d prefix from all names and used a common lisp package instead.
  - I dropped all type suffixes from enum- and function names, in a
    dynamic typed language they make less sense.
  - I `lispified' all symbols (downcases and hyphens instead of camel case)
  - I dropped all -Element suffixes from enums (debateable)
  - I adhered to the common-lisp tradition of not using abbreviations,
    so I renamed all occurrences of
    + 'res' to resource
    + 'mat' to 'material' (or 'matrix')
    + 'vert' to 'vertex'
    + 'elem' to 'element'
    + 'comp' to 'component'
    + 'tex' to 'texture'

Running the examples 
~~~~~~~~~~~~~~~~~~~~~
  
  - Load the horde3d-examples system

  - adjust the *horde3d-home-directory* variable in src/examples.lisp
    to point to your Horde3D installation.

  - run (horde3d-examples:knight) or (horde3d-examples:chicago)

