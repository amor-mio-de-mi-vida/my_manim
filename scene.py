from manim import *
from validators.i18n import ind


class CreateCircle(Scene):
    def construct(self):
        circle = Circle()  # create a circle
        circle.set_fill(PINK, opacity=0.5)  # set the color and transparency
        self.play(Create(circle))  # show the circle on screen


class SquareToCircle(Scene):
    def construct(self):
        circle = Circle()
        circle.set_fill(PINK, opacity=0.5)

        square = Square()
        square.rotate(PI / 4)

        self.play(Create(square))
        self.play(Transform(square, circle))
        self.play(FadeOut(square))


class SquareAndCircle(Scene):
    def construct(self):
        circle = Circle()
        circle.set_fill(PINK, opacity=0.5)

        square = Square()
        square.set_fill(BLUE, opacity=0.5)

        square.next_to(circle, RIGHT, buff=0.5)
        self.play(Create(circle), Create(square))


class AnimatedSquareToCircle(Scene):
    def construct(self):
        circle = Circle()
        square = Square()

        self.play(Create(square))
        self.play(square.animate.rotate(PI / 4))
        self.play(Transform(square, circle))
        self.play(
            square.animate.set_fill(PINK, opacity=0.5)
        )


class DifferentRotations(Scene):
    def construct(self):
        left_square = Square(color=BLUE, fill_opacity=0.7).shift(2 * LEFT)
        right_square = Square(color=GREEN, fill_opacity=0.7).shift(2 * RIGHT)
        self.play(
            left_square.animate.rotate(PI), Rotate(right_square, angle=PI), run_time=2
        )
        self.wait()


class TwoTransforms(Scene):
    def transform(self):
        a = Circle()
        b = Square()
        c = Triangle()
        self.play(Transform(a, b))
        self.play(Transform(a, c))
        self.play(FadeOut(a))

    def replacement_transform(self):
        a = Circle()
        b = Square()
        c = Triangle()
        self.play(ReplacementTransform(a, b))
        self.play(ReplacementTransform(b, c))
        self.play(FadeOut(c))

    def construct(self):
        self.transform()
        self.wait(0.5)
        self.replacement_transform()


class TestSection(Scene):
    def construct(self):
        self.add(Circle())
        self.next_section()


###################  Creating a custom animation  ###################
class Count(Animation):
    def __init__(self, number: DecimalNumber, start: float, end: float, **kwargs) -> None:
        super().__init__(number, **kwargs)
        self.start = start
        self.end = end

    def interpolate(self, alpha: float) -> None:
        value = self.start + (alpha * (self.end - self.start))
        self.mobject.set_value(value)


class CountingScene(Scene):
    def construct(self):
        number = DecimalNumber().set_color(WHITE).scale(5)
        # Add an updater to keep the DecimalNumber centered as its value changes
        number.add_updater(lambda number: number.move_to(ORIGIN))

        self.add(number)

        self.wait()

        self.play(Count(number, 0, 100), run_time=4, rate_func=linear)

        self.wait()


class MobjectExample(Scene):
    def construct(self):
        p1 = [-1, -1, 0]
        p2 = [1, -1, 0]
        p3 = [1, 1, 0]
        p4 = [-1, 1, 0]
        a = Line(p1, p2).append_points(Line(p2, p3).points).append_points(Line(p3, p4).points)
        point_start = a.get_start()
        point_end = a.get_end()
        point_center = a.get_center()
        self.add(
            Text(f"a.get_start() = {np.round(point_start, 2).tolist()}", font_size=24).to_edge(UR).set_color(YELLOW))
        self.add(Text(f"a.get_end() = {np.round(point_end, 2).tolist()}", font_size=24).next_to(self.mobjects[-1],
                                                                                                DOWN).set_color(RED))
        self.add(Text(f"a.get_center() = {np.round(point_center, 2).tolist()}", font_size=24).next_to(self.mobjects[-1],
                                                                                                      DOWN).set_color(
            BLUE))

        self.add(Dot(a.get_start()).set_color(YELLOW).scale(2))
        self.add(Dot(a.get_end()).set_color(RED).scale(2))
        self.add(Dot(a.get_top()).set_color(GREEN_A).scale(2))
        self.add(Dot(a.get_bottom()).set_color(GREEN_D).scale(2))
        self.add(Dot(a.get_center()).set_color(BLUE).scale(2))
        self.add(Dot(a.point_from_proportion(0.5)).set_color(ORANGE).scale(2))
        self.add(*[Dot(x) for x in a.points])
        self.add(a)


class ExampleTransform(Scene):
    def construct(self):
        self.camera.background_color = WHITE
        m1 = Square().set_color(RED)
        m2 = Rectangle().set_color(RED).rotate(0.2)
        self.play(Transform(m1, m2))


class ExampleRotation(Scene):
    def construct(self):
        self.camera.background_color = WHITE
        m1a = Square().set_color(RED).shift(LEFT)
        m1b = Circle().set_color(RED).shift(LEFT)
        m2a = Square().set_color(BLUE).shift(RIGHT)
        m2b = Circle().set_color(BLUE).shift(RIGHT)

        points = m2a.points
        points = np.roll(points, int(len(points) / 4), axis=0)
        # 翻转各个点
        m2a.points = points

        self.play(Transform(m1a, m1b), Transform(m2a, m2b), run_time=1)


class ShowScreenResolution(Scene):
    def construct(self):
        pixel_height = config["pixel_height"]
        pixel_width = config["pixel_width"]
        frame_width = config["frame_width"]
        frame_height = config["frame_height"]
        self.add(Dot())
        d1 = Line(frame_width * LEFT / 2, frame_width * RIGHT / 2).to_edge(DOWN)
        self.add(d1)
        self.add(Text(str(pixel_width)).next_to(d1, UP))
        d2 = Line(frame_height * UP / 2, frame_height * DOWN / 2).to_edge(LEFT)
        self.add(d2)
        self.add(Text(str(pixel_height)).next_to(d2, RIGHT))


class ToyExample(Scene):
    def construct(self):
        orange_square = Square(color=ORANGE, fill_opacity=0.5)
        blue_circle = Circle(color=BLUE, fill_opacity=0.5)
        self.add(orange_square)
        self.play(ReplacementTransform(orange_square, blue_circle, run_time=3))
        small_dot = Dot()
        small_dot.add_updater(lambda mob: mob.next_to(blue_circle, DOWN))
        self.play(Create(small_dot))
        self.play(blue_circle.animate.shift(RIGHT))
        self.wait()
        self.play(FadeOut(blue_circle, small_dot))


class VMobjectDemo(Scene):
    def construct(self):
        plane = NumberPlane()
        my_vmobject = VMobject(color=GREEN)
        my_vmobject.points = [
            np.array([-2, -1, 0]),
            np.array([-3, 1, 0]),
            np.array([0, 3, 0]),
            np.array([1, 3, 0]),
            np.array([1, 3, 0]),
            np.array([0, 1, 0]),
            np.array([4, 3, 0]),
            np.array([4, -2, 0]),
        ]
        handles = [
            Dot(point, color=RED) for point in
            [[-3, 1, 0], [0, 3, 0], [0, 1, 0], [4, 3, 0]]
        ]
        handle_lines = [
            Line(
                my_vmobject.points[ind],
                my_vmobject.points[ind + 1],
                color=RED,
                stroke_width=2
            ) for ind in range(0, len(my_vmobject.points), 2)
        ]
        self.add(plane, *handles, *handle_lines, my_vmobject)


class AddPackageLatex(Scene):
    def construct(self):
        myTemplate = TexTemplate()
        myTemplate.add_to_preamble(r"\usepackage{mathrsfs}")
        tex = Tex(
            r"$\mathscr{H} \rightarrow \mathbb{H}$",
            tex_template=myTemplate,
            font_size=144,
        )
        self.add(tex)


class LaTeXSubstrings(Scene):
    def construct(self):
        tex = Tex('Hello', r'$\bigstar$', r'\LaTeX', font_size=144)
        tex.set_color_by_tex('bigstar', RED)
        self.add(tex)


class IndexLabelsMathTex(Scene):
    def construct(self):
        text = MathTex(r"\binom{2n}{n+2}", font_size=96)

        self.add(index_labels(text[0]))

        text[0][1:3].set_color(YELLOW)
        text[0][3:6].set_color(RED)
        self.add(text)


class LaTeXMathFonts(Scene):
    def construct(self):
        tex = Tex(
            r"$x^2 + y^2 = z^2$",
            tex_template=TexFontTemplates.french_cursive,
            font_size=144
        )
        self.add(tex)


class LaTeXTemplateLibrary(Scene):
    def construct(self):
        tex = Tex('Hello 你好 \\LaTeX', tex_template=TexTemplateLibrary.ctex, font_size=144)
        self.add(tex)


class LaTeXAlignEnvironment(Scene):
    def construct(self):
        tex = MathTex(r"f(x) &= 3 + 2 + 1\\ &= 5 + 1\\ &= 6", font_size=96)
        self.add(tex)


class SecondExample(Scene):
    def construct(self):
        ax = Axes(x_range=(-3, 3), y_range=(-3, 3))
        curve = ax.plot(lambda x: (x + 2) * x * (x - 2) / 2, color=RED)
        area = ax.get_area(curve, x_range=(-2, 0))
        self.play(Create(ax), Create(curve))
        self.play(FadeIn(area))
        self.wait(2)


class SquareToCircle2(Scene):
    def construct(self):
        green_square = Square(color=GREEN, fill_opacity=0.5)
        self.play(DrawBorderThenFill(green_square))
        blue_circle = Circle(color=BLUE, fill_opacity=0.5)
        self.play(ReplacementTransform(green_square, blue_circle, run_time=3))
        self.play(Indicate(blue_circle))
        self.play(FadeOut(blue_circle))


class Positioning(Scene):
    def construct(self):
        plane = NumberPlane()
        self.add(plane)

        # next_to from episode 1
        red_dot = Dot(color=RED)
        green_dot = Dot(color=GREEN)
        green_dot.next_to(red_dot, RIGHT + UP)
        self.add(red_dot, green_dot)

        # shift
        s = Square(color=ORANGE)
        s.shift(2 * UP + 4 * RIGHT)
        self.add(s)

        # move_to
        c = Circle(color=PURPLE)
        c.move_to([-3, -2, 0])
        self.add(c)

        # align_to
        c2 = Circle(radius=0.5, color=RED, fill_opacity=0.5)
        c3 = c2.copy().set_color(YELLOW)
        c4 = c2.copy().set_color(ORANGE)
        c2.align_to(s, UP)
        c3.align_to(s, RIGHT)
        c4.align_to(s, UP + RIGHT)
        self.add(c2, c3, c4)


class CriticalPoints(Scene):
    def construct(self):
        c = Circle(color=GREEN, fill_opacity=0.5)
        self.add(c)

        for d in [(0, 0, 0), UP, UR, RIGHT, DR, DOWN, DL, LEFT, UL]:
            self.add(Cross(scale_factor=0.2).move_to(c.get_critical_point(d)))

        s = Square(color=RED, fill_opacity=0.5)
        s.move_to([1, 0, 0], aligned_edge=LEFT)
        self.add(s)


from manim.utils.unit import Percent, Pixels


class UsefulUnits(Scene):
    def construct(self):
        for perc in range(5, 51, 5):
            self.add(Circle(radius=perc * Percent(X_AXIS)))
            self.add(Square(side_length=2 * perc * Percent(Y_AXIS), color=YELLOW))

        d = Dot()
        d.shift(100 * Pixels * RIGHT)
        self.add(d)


class Grouping(Scene):
    def construct(self):
        red_dot = Dot(color=RED)
        green_dot = Dot(color=GREEN).next_to(red_dot, RIGHT)
        blue_dot = Dot(color=BLUE).next_to(red_dot, UP)
        dot_group = VGroup(red_dot, green_dot, blue_dot)
        dot_group.to_edge(RIGHT)
        self.add(dot_group)

        circles = VGroup(*[Circle(radius=0.2) for _ in range(10)])
        circles.arrange(UP)
        self.add(circles)

        stars = VGroup(*[Star(color=YELLOW, fill_opacity=1).scale(0.5) for _ in range(20)])
        stars.arrange_in_grid(4, 5, buff=0.2)
        self.add(stars)


from colour import Color


##### ? #####
class BasicAnimations(Scene):
    def construct(self):
        polys = VGroup(
            *[RegularPolygon(5, radius=1, fill_opacity=0.5,
                             color=Color(hue=1, saturation=1, luminance=0.5)) for j in range(1)]
        ).arrange(RIGHT)
        self.play(DrawBorderThenFill(polys), run_time=2)
        self.play(
            Rotate(polys[0], PI, rate_func=lambda t: t),
            Rotate(polys[1], PI, rate_func=smooth),
            Rotate(polys[2], PI, rate_func=lambda t: np.sin(t * PI)),
            Rotate(polys[3], PI, rate_func=there_and_back),
            Rotate(polys[4], PI, rate_func=lambda t: 1 - abs(1 - 2 * t)),
            run_time=2,
        )
        self.wait()


##### ? #####
class LaggingGroup(Scene):
    def construct(self):
        squares = VGroup(*[Square(color=Color(hue=j / 20, saturation=1, luminance=0.5),
                                  fill_opacity=0.5) for j in range(20)]).arrange_in_grid(4, 5).scale(0.75)
        self.play(AnimationGroup(*[FadeIn(s) for s in squares], lag_ratio=1))


class AnimateSyntax(Scene):
    def construct(self):
        s = Square(color=GREEN, fill_opacity=0.5)
        c = Circle(color=RED, fill_opacity=0.5)
        self.add(s, c)

        self.play(s.animate.shift(UP), c.animate.shift(DOWN))
        self.play(VGroup(s, c).animate.arrange(RIGHT, buff=1))
        self.play(c.animate(rate_func=linear).shift(RIGHT).scale(2))


class AnimateProblem(Scene):
    def construct(self):
        left_square = Square()
        right_square = Square()
        VGroup(left_square, right_square).arrange(RIGHT, buff=1)
        self.add(left_square, right_square)
        self.play(left_square.animate.rotate(PI), Rotate(right_square, PI), run_time=2)
        self.wait()


class AnimationMechanisms(Scene):
    def construct(self):
        c = Circle()

        c.generate_target()
        c.target.set_fill(color=GREEN, opacity=0.5)
        c.target.shift(2 * RIGHT + UP).scale(0.5)

        self.add(c)
        self.wait()
        self.play(MoveToTarget(c))

        s = Square()
        s.save_state()
        self.play(s.animate.set_color(PURPLE).set_opacity(0.5).shift(2 * LEFT).scale(3))
        self.play(s.animate.shift(5 * DOWN).rotate(PI / 4))
        self.wait()
        self.play(Restore(s), run_time=2)
        self.wait()


##### ? #####
class SimpleCustomAnimation(Scene):
    def construct(self):
        def spiral_out(mobject, t):
            radius = 4 * t
            angle = 2 * t * 2 * PI
            mobject.move_to(radius * (np.cos(angle) * RIGHT + np.sin(angle) * UP))
            mobject.set_color(Color(hue=t, saturation=1, luminance=0.5))
            mobject.set_opacity(1 - t)

        d = Dot(color=WHITE)
        self.add(d)
        self.play(UpdateFromAlphaFunc(d, spiral_out, run_time=3))


class Disperse(Animation):
    def __init__(self, mobject, dot_radius=0.05, dot_number=100, **kwargs):
        super().__init__(mobject, **kwargs)
        self.dots = None
        self.dot_radius = dot_radius
        self.dot_number = dot_number

    def begin(self):
        dots = VGroup(
            *[Dot(radius=self.dot_radius).move_to(self.mobject.point_from_proportion(p))
              for p in np.linspace(0, 1, self.dot_number)]
        )
        for dot in dots:
            dot.initial_position = dot.get_center()
            dot.shift_vector = 2 * (dot.get_center() - self.mobject.get_center())
        dots.set_opacity(0)
        self.mobject.add(dots)
        self.dots = dots
        super().begin()

    def clean_up_from_scene(self, scene: Scene) -> None:
        super().clean_up_from_scene(scene)
        scene.remove(self.dots)

    def interpolate_mobject(self, alpha):
        alpha = self.rate_func(alpha)
        if alpha <= 0.5:
            self.mobject.set_opacity(1 - 2 * alpha, family=False)
            self.dots.set_opacity(2 * alpha)
        else:
            self.mobject.set_opacity(0)
            self.dots.set_opacity(2 * (1 - alpha))
            for dot in self.dots:
                dot.move_to(dot.initial_position + 2 * (alpha - 0.5) * dot.shift_vector)


class CustomAnimation(Scene):
    def construct(self):
        st = Star(color=YELLOW, fill_opacity=1).scale(3)
        self.add(st)
        self.wait()
        self.play(Disperse(st, dot_number=200, run_time=4))


################### episode 4 #######################
class TestUpdated(Scene):
    def construct(self):
        blue_dot = Dot(color=BLUE)
        dot_label = Text("Hello dot!").next_to(blue_dot, UP)
        dot_label.add_updater(
            lambda mobject: mobject.next_to(blue_dot, UP)
        )
        self.add(blue_dot, dot_label)
        self.play(blue_dot.animate.shift(RIGHT))
        self.play(blue_dot.animate.scale(3))
        self.play(blue_dot.animate.center())


class AllUpdaterTypes(Scene):
    def construct(self):
        red_dot = Dot(color=RED).shift(LEFT)
        pointer = Arrow(ORIGIN, RIGHT).next_to(red_dot, LEFT)
        pointer.add_updater(
            lambda mobject: mobject.next_to(red_dot, LEFT)
        )

        def shifter(mob, dt):
            mob.shift(2 * dt * RIGHT)

        red_dot.add_updater(shifter)

        def scene_scaler(dt):
            for mob in self.mobjects:
                mob.set(width=2 / (1 + np.linalg.norm(mob.get_center())))

        self.add_updater(scene_scaler)

        self.add(red_dot, pointer)
        self.update_self(0)
        self.wait(5)


class UpdaterAndAnimation(Scene):
    def construct(self):
        red_dot = Dot(color=RED).shift(LEFT)
        rotating_square = Square()
        rotating_square.add_updater(
            lambda mob, dt: mob.rotate(PI * dt)
        )

        def shifter(mob, dt):
            mob.shift(2 * dt * RIGHT)

        red_dot.add_updater(shifter)

        self.add(red_dot, rotating_square)
        self.wait(1)
        red_dot.suspend_updating()
        self.wait(1)

        self.play(
            red_dot.animate.shift(UP),
            rotating_square.animate.move_to([-2, -2, 0])
        )
        self.wait(10)


class TestValueTracker(Scene):
    def construct(self):
        line = NumberLine(x_range=[-5, 5])
        position = ValueTracker(0)
        pointer = Vector(DOWN)
        pointer.add_updater(
            lambda mob: mob.next_to(
                line.number_to_point(position.get_value()), UP
            )
        )
        pointer.update()
        self.add(line, pointer)
        self.wait()
        self.play(position.animate.set_value(4))
        self.play(position.animate.set_value(-2))


class ValueTrackerPlot(Scene):
    def construct(self):
        a = ValueTracker(1)
        ax = Axes(x_range=[-2, 2, 1], y_range=[-8.5, 8.5, 1], x_length=4, y_length=6)
        parabola = ax.plot(lambda x: x ** 2, color=RED)
        parabola.add_updater(
            lambda mob: mob.become(ax.plot(lambda x: a.get_value() * x ** 2, color=RED))
        )
        a_number = DecimalNumber(
            a.get_value(),
            color=RED,
            num_decimal_places=3,
            show_ellipsis=True
        )
        a_number.add_updater(
            lambda mob: mob.set_value(a.get_value()).next_to(parabola, RIGHT)
        )
        self.add(ax, parabola, a_number)
        self.play(a.animate.set_value(2))
        self.wait(1)
        self.play(a.animate.set_value(-2))
        self.wait(1)
        self.play(a.animate.set_value(1))
        self.wait(1)


from manim.opengl import *


class OpenGLIntro(Scene):
    def construct(self):
        hello_world = Tex("Hello World!").scale(3)
        self.play(Write(hello_world))
        self.play(self.camera.animate.set_euler_angles(
            theta=-10 * DEGREES,
            phi=50 * DEGREES
        )
        )
        self.play(FadeOut(hello_world))
        surface = OpenGLSurface(
            lambda u, v: (u, v, u * np.sin(v) + v * np.cos(u)),
            u_range=(-3, 3),
            v_range=(-3, 3),
        )
        surface_mesh = OpenGLSurfaceMesh(surface)
        self.play(Create(surface_mesh))
        self.play(FadeTransform(surface_mesh, surface))
        self.wait()

        light = self.camera.light_source
        self.play(light.animate.shift([0, 0, -20]))
        self.play(light.animate.shift([0, 0, 10]))
        self.play(self.camera.animate.set_euler_angles(theta=60 * DEGREES))
        self.interactive_embed()
        # self.wait()


class InterativeRadius(Scene):
    def construct(self):
        plane = NumberPlane()
        cursor_dot = Dot().move_to(3 * RIGHT + 2 * UP)
        red_circle = Circle(
            radius=np.linalg.norm(cursor_dot.get_center()),
            color=RED
        )
        red_circle.add_updater(
            lambda mob: mob.become(
                Circle(
                    radius=np.linalg.norm(cursor_dot.get_center()),
                    color=RED
                )
            )
        )
        self.play(Create(plane), Create(red_circle), FadeIn(cursor_dot))
        self.cursor_dot = cursor_dot
        self.interactive_embed()

    def on_key_press(self, symbol, modifiers):
        from pyglet.window import key as pyglet_key
        if symbol == pyglet_key.G:
            self.play(
                self.cursor_dot.animate.move_to(self.mouse_point.get_center())
            )
        super().on_key_press(symbol, modifiers)


######## ？ #######
class NewtonIteration(Scene):
    def construct(self):
        self.axes = Axes()
        self.f = lambda x: (x + 6) * (x + 3) * x * (x - 3) * (x - 6) / 300
        curve = self.axes.plot(self.f, color=RED)
        self.cursor_dot = Dot(color=YELLOW)
        self.play(Create(self.axes), Create(curve), FadeIn(self.cursor_dot))
        self.interactive_embed()

    def on_key_press(self, symbol, modifiers):
        from pyglet.window import key as pyglet_key
        from scipy.misc import derivative
        if symbol == pyglet_key.P:
            x, y = self.axes.point_to_coords(self.mouse_point.get_center())
            self.play(
                self.cursor_dot.animate.move_to(self.axes.c2p(x, self.f(x)))
            )

        if symbol == pyglet_key.I:
            x, y = self.axes.point_to_coords(self.cursor_dot.get_center())
            # Newton iteration: x_new = x - f(x) / f'(x)
            x_new = x - self.f(x) / derivative(self.f, x, dx=0.01)
            curve_point = self.cursor_dot.get_center()
            axes_point = self.axes.c2p(x_new, 0)
            tangent = Line(
                curve_point + (curve_point - axes_point) * 0.25,
                axes_point + (axes_point - curve_point) * 0.25,
                color=YELLOW,
                stroke_width=2
            )
            self.play(Create(tangent))
            self.play(self.cursor_dot.animate.move_to(self.axes.c2p(x_new, 0)))
            self.play(
                self.cursor_dot.animate.move_to(self.axes.c2p(x_new, self.f(x_new))),
                FadeOut(tangent)
            )


import random


class SweepingLine(Scene):
    def construct(self):
        growing_circle = Circle(radius=0.01)

        moving_line = Line([-7, -5, 0], [-6, 5, 0])
        moving_line.normal_vector = moving_line.copy().rotate(90 * DEGREES).get_vector()

        def opacity_updater(obj):
            if (
                    sum((growing_circle.points[0] - growing_circle.get_center()) ** 2)
                    >= sum((obj.get_center() - growing_circle.get_center()) ** 2)
                    # round (
                    #     get_winding_number(growing_circle.get_anchors() - obj.get_center())
                    # ) > 0
            ):
                obj.set_fill(BLUE, opacity=1)
                obj.clear_updaters()
                obj.add_updater(color_updater)
                # self.add_sound("assets/click.wav")

        def color_updater(obj):
            if (
                    np.dot(obj.get_center(), moving_line.normal_vector)
                    < np.dot(moving_line.get_start(), moving_line.normal_vector)
            ):
                if obj.color != BLUE:
                    obj.set_color(BLUE)
                    # self.add_sound("assets/click.wav")
            else:
                if obj.color != YELLOW:
                    obj.set_color(YELLOW)
                    # self.add_sound("assets/click.wav")

        self.add(growing_circle)

        for _ in range(30):
            p = Dot(fill_opacity=0.5)
            p.move_to([random.uniform(-6, 6), random.uniform(-4, 4), 0])
            p.add_updater(opacity_updater)
            self.add(p)

        self.play(
            growing_circle.animate.scale_to_fit_width(1.5 * config.frame_width),
            run_time=5
        )
        self.play(Create(moving_line))
        self.play(moving_line.animate.shift(14 * RIGHT), run_time=5)
        self.play(moving_line.animate.shift(14 * LEFT), run_time=5)

class TexOpacityExample(Scene):
    def construct(self):
        equation = MathTex(
            r"\Gamma(1-z) \Gamma(z) = \frac{\pi}{\sin \pi z}",
            substrings_to_isolate=["z"]
        ).scale(2)
        self.play(Write(equation))
        self.wait()
        equation.save_state()
        self.play(
            equation.animate.set_opacity_by_tex("z", opacity=1, remaining_opacity=0.25)
        )
        self.wait()
        self.play(Restore(equation))

class CircleInGrid(Scene):
    def construct(self):
        dots = VGroup(*[
            Dot() for _ in range(16*9)
        ]).arrange_in_grid(9, 16, buff=1)
        self.add(dots)

        growing_circle = Circle(0.1, color=PURPLE_D, fill_opacity=0.25).shift(0.1*LEFT+0.3*UP)
        self.add(growing_circle)
        growing_circle.add_updater(lambda c, dt: c.scale_to_fit_width(c.width + dt))

        def number_of_dots_in_circle():
            return len([
                dot for dot in dots
                if np.linalg.norm(dot.get_center() - growing_circle.get_center()) < growing_circle.width/2
            ])

        for _ in range(15):
            dots_inside = number_of_dots_in_circle()
            self.wait_until(lambda: number_of_dots_in_circle() != dots_inside)
            growing_circle.suspend_updating()
            self.wait(0.5)
            growing_circle.resume_updating()

class RoundedPolyrams(Scene):
    def construct(self):
        st1 = Star().scale(2).round_corners((0.4, 0.1))
        self.add(st1)


if __name__ == "__main__":
    CustomAnimation().construct()
