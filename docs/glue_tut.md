# Gluing together animations

Reanimate is a library for programmatically generating animations with a twist
towards mathematics / vector drawings. A lot of inspiration was drawn from
[3b1b's](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw) [manim](https://github.com/3b1b/manim) library.

Reanimate aims at being a batteries-included way of gluing together different technologies: SVG as a universal image format, LaTeX for typesetting, ffmpeg for video encoding, inkscape/imagemagick for rasterization, potrace for vectorization, blender/povray for 3D graphics, and Haskell for scripting.

## Scalable Vector Graphics

Movies consists of a sequence of frames and, in reanimate, these frames are [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Introduction) images. SVGs can easily reference raster images, includes a set of drawing primitives, and offers image advanced manipulation through filter effects.
Since SVGs are plain-text documents, tools can be written to analyse and modify images. For example, reanimate includes code for applying 2D physics to shapes in SVG images.

SVG features, as demonstrated in the below animation:

 * Drawing primitives: Circles, rectangles, lines, external images, paths, text.
 * Drawing attributes: Rotation, position, color, line-width.
 * Filter effects: Blur and blob (merging shapes).

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_svg.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/KjLoVZf.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_svg.mp4">
</video>

## Animation = Time ➞ SVG

Animations can be defined as SVG images over time (plus a bit of bookkeeping such as their duration). With this approach, the time variable can determine SVG properties such as radius, path lengths, rotation, and color. Reanimate ships with a bunch of combinators for composing and arranging animations.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_animate.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/qVNDt43.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_animate.mp4">
</video>

Reanimate is not an opinionated framework, though, and also offers a more traditional keyframing tools. The example below uses an imperative API to schedule the various sub animations, transitions, and effects.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_keyframe.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/jdEtt4b.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_keyframe.mp4">
</video>

## Pillar I: Haskell

A large part of Reanimate's expressive power comes from using Haskell as the scripting language. Haskell tends to favor expressiveness and correctness over raw performance and that is exactly what is needed from a glue language. All the heavy-lifting of rendering frames and encoding videos is handled by external tools and Reanimate merely needs to concern itself with finding intuitive ways of describing animations.

The following examples shows how something as seemingly complicated as fourier series can be expressed and animated in Haskell. Most noteworthy is the layered structure of the code:

 1. The first layer handles the mathematics of fourier series without worrying about graphics or how properties should be animated,
 2. the second layer describes how to draw a single frame given the length of the fourier series and the degree of rotation,
 3. the third layer deals with time: order of animations, durations, number of rotations, transition timing functions, pauses, etc.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_fourier.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/lvVhMXn.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_fourier.mp4">
</video>

Scripting in Haskell also gives access to the extensive body of code libraries. There are Haskell libraries for syntax highlighting, font manipulation, and much, much more. In the spirit of being a batteries-included framework, Reanimate ships with a built-in 2D physics library, called [Chipmunk2D](https://chipmunk-physics.net/). The example below demonstrates how SVG shapes can be used nearly effortlessly in a physics simulation.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!reanimate-examples/tut_glue_physics.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/S6xfCl9.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_physics.mp4">
</video>


## Pillar II: LaTeX


[LaTeX](https://www.latex-project.org/) is a widely used system for typesetting equations and documents. It is most commonly used by writing TeX documents which are then converted to pdfs. However, since the output of LaTeX is natively vector graphics, it is trivial to get SVG documents instead of pdfs. Armed with this knowledge, Reanimate offers a simple yet powerful function: `latex :: Text -> SVG`

The `latex` function takes a snippet of TeX code, passes it through the LaTeX system, and converts the result to an SVG image. Furthermore, since the result is entirely determined by the TeX code, caching is used to hide the overhead of invoking LaTeX.

The resulting SVGs can be manipulated just like any other. The below examples illustrates how different effects can be applied to different glyphs in the equation.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_latex.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/uZns44f.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_latex.mp4">
</video>

## Pillar III: povray

Although incredibly expressive, SVGs are strictly limited to 2D graphics. This limitation can be overcome with a 3D renderer such as [povray](https://www.povray.org/): povray is a nearly 30 year-old raytracer with a relatively small but solid set of features. Reanimate offers convenient functions for importing povray scenes as well as exporting animations to be used as textures. In the video below, the LaTeX animation is projected upon a plane which is then rotated and translated in 3D space. A key thing to note is that both the 2D and 3D elements are managed entirely through code.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_povray.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/7nty7c5.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_povray.mp4">
</video>


The video above uses a perspective camera, ie. objects further away appears to be smaller. This gives the appearance of three dimensions but it also makes it difficult to interlace SVG objects and 3D objects with pixel-perfect precision. For example, aligning a cube and a square requires the exact position of the pinhole camera. All of this can be dramatically simplified with an [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection) where 'x' an 'y' coordinates in 3D space always map to the same 'x' and 'y' coordinates on the screen. Shapes lose their perspective but in many cases, especially when illustrating mathematical concepts, drawing "idealized" shapes is perfectly fine. The video below shows an orthographic projection of a sphere. The sphere (3D shape) could be completely eclipsed by a circle (2D shape) of the same radius with pixel-perfect accuracy.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_povray_ortho.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/uTrsxvm.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_povray_ortho.mp4">
</video>


## Pillar IV: Blender

[Blender](https://www.blender.org/) is a vastly more modern and capable 3D modeller than povray but has a slightly steeper learning curve. Most people interact with Blender through a graphical user interface but all of blender's features can also be used directly from Python. The [Python API](https://docs.blender.org/api/current/index.html) is sizable and, at first glance, it might seem that there are almost no tutorials for blender scripting. However, Blender can tell you the Python command for every action in the GUI, making it easy to translate graphical tutorials to scripting tutorials.

The example below uses built-in modifiers to bend a plane into a sphere and is rendered using the EEVEE engine.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_blender.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/i1xtZgl.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_blender.mp4">
</video>

## Pillar V: potrace

[Potrace](https://en.wikipedia.org/wiki/Potrace) takes pixel data (from an image file, or perhaps generated by povray or Blender) and automatically convert it to vector graphics. Once an image has been vectorized, it can be manipulated with the standard SVG tools. In the example below, a sphere is rendered with povray, vectorized, and then line-drawn.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_potrace.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%;max-width:640px" muted autoplay loop>
  <source src="https://i.imgur.com/6C3pMvc.mp4">
  <source src="https://f002.backblazeb2.com/file/reanimate/tut_glue_potrace.mp4">
</video>
