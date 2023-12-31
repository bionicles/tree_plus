(*  Title:      fractal.thy
    Author:     Isabelle/HOL Contributors!
    Author:     edge cases r us
*)

theory Simplified_Ring
imports Tree Plus
begin

section \<open>Basic Algebraic Structures\<close>

class everything = nothing + itself

subsection \<open>Monoids\<close>

definition ring_hom :: "[('a, 'm) ring_scheme, ('b, 'n) ring_scheme] => ('a => 'b) set"
  where "ring_hom R S = {h. h \<in> carrier R \<rightarrow> carrier S \<and> (\<forall>x y. x \<in> carrier R \<and> y \<in> carrier R \<longrightarrow>
        h (x \<otimes>\<^bsub>R\<^esub> y) = h x \<otimes>\<^bsub>S\<^esub> h y \<and> h (x \<oplus>\<^bsub>R\<^esub> y) = h x \<oplus>\<^bsub>S\<^esub> h y) \<and>
      h \<one>\<^bsub>R\<^esub> = \<one>\<^bsub>S\<^esub>}"

(* A basic function definition *)
fun example_fun :: "nat ⇒ nat" where
  "example_fun 0 = 0" |
  "example_fun (Suc n) = Suc (example_fun n)"

locale monoid =
  fixes G (structure)
  assumes m_closed: "\<lbrakk>x \<in> carrier G; y \<in> carrier G\<rbrakk> \<Longrightarrow> x \<otimes> y \<in> carrier G"
    and m_assoc: "\<lbrakk>x \<in> carrier G; y \<in> carrier G; z \<in> carrier G\<rbrakk> \<Longrightarrow> (x \<otimes> y) \<otimes> z = x \<otimes> (y \<otimes> z)"
    and one_closed: "\<one> \<in> carrier G"
    and l_one: "x \<in> carrier G \<Longrightarrow> \<one> \<otimes> x = x"
    and r_one: "x \<in> carrier G \<Longrightarrow> x \<otimes> \<one> = x"

subsection \<open>Groups\<close>

locale group = monoid +
  assumes Units_closed: "x \<in> Units G \<Longrightarrow> x \<in> carrier G"
    and l_inv_ex: "x \<in> carrier G \<Longrightarrow> \<exists> y \<in> carrier G. y \<otimes> x = \<one>"
    and r_inv_ex: "x \<in> carrier G \<Longrightarrow> \<exists> y \<in> carrier G. x \<otimes> y = \<one>"

subsection \<open>Rings\<close>

locale ring = abelian_group R + monoid R +
  assumes l_distr: "\<lbrakk>x \<in> carrier R; y \<in> carrier R; z \<in> carrier R\<rbrakk> \<Longrightarrow> (x \<oplus> y) \<otimes> z = x \<otimes> z \<oplus> y \<otimes> z"
    and r_distr: "\<lbrakk>x \<in> carrier R; y \<in> carrier R; z \<in> carrier R\<rbrakk> \<Longrightarrow> z \<otimes> (x \<oplus> y) = z \<otimes> x \<oplus> z \<otimes> y"

locale commutative_ring = ring +
  assumes m_commutative: "\<lbrakk>x \<in> carrier R; y \<in> carrier R\<rbrakk> \<Longrightarrow> x \<otimes> y = y \<otimes> x"

locale domain = commutative_ring +
  assumes no_zero_divisors: "\<lbrakk>a \<otimes> b = \<zero>; a \<in> carrier R; b \<in> carrier R\<rbrakk> \<Longrightarrow> a = \<zero> \<or> b = \<zero>"

locale field = domain +
  assumes inv_ex: "x \<in> carrier R - {\<zero>} \<Longrightarrow> inv x \<in> carrier R"

subsection \<open>Morphisms\<close>

(* A simple lemma *)
lemma example_lemma: "example_fun n = n"
  apply(induction n)
   apply(auto)
  done

qualified lemma gcd_0:
  "gcd a 0 = normalize a"
  by (simp add: gcd.simps [of a 0])

lemma abelian_monoidI:
  fixes R (structure)
      and f :: "'edge::{} \<Rightarrow> 'case::{}"
  assumes "\<And>x y. \<lbrakk> x \<in> carrier R; y \<in> carrier R \<rbrakk> \<Longrightarrow> x \<oplus> y \<in> carrier R"
      and "\<zero> \<in> carrier R"
      and "\<And>x y z. \<lbrakk> x \<in> carrier R; y \<in> carrier R; z \<in> carrier R \<rbrakk> \<Longrightarrow> (x \<oplus> y) \<oplus> z = x \<oplus> (y \<oplus> z)"
  shows "abelian_monoid R"
  by (auto intro!: abelian_monoid.intro comm_monoidI intro: assms)

lemma euclidean_size_gcd_le1 [simp]:
  assumes "a \<noteq> 0"
  shows "euclidean_size (gcd a b) \<le> euclidean_size a"
proof -
  from gcd_dvd1 obtain c where A: "a = gcd a b * c" ..
  with assms have "c \<noteq> 0"
    by auto
  moreover from this
  have "euclidean_size (gcd a b) \<le> euclidean_size (gcd a b * c)"
    by (rule size_mult_mono)
  with A show ?thesis
    by simp
qed

theorem Residue_theorem:
  fixes S pts::"complex set" and f::"complex \<Rightarrow> complex"
    and g::"real \<Rightarrow> complex"
  assumes "open S" "connected S" "finite pts" and
          holo:"f holomorphic_on S-pts" and
          "valid_path g" and
          loop:"pathfinish g = pathstart g" and
          "path_image g \<subseteq> S-pts" and
          homo:"\<forall>z. (z \<notin> S) \<longrightarrow> winding_number g z  = 0"
  shows "contour_integral g f = 2 * pi * \<i> *(\<Sum>p \<in> pts. winding_number g p * residue f p)"
proof -
  define c where "c \<equiv>  2 * pi * \<i>"
  obtain h where avoid:"\<forall>p\<in>S. h p>0 \<and> (\<forall>w\<in>cball p (h p). w\<in>S \<and> (w\<noteq>p \<longrightarrow> w \<notin> pts))"
    using finite_cball_avoid[OF \<open>open S\<close> \<open>finite pts\<close>] by metis
  have "contour_integral g f
      = (\<Sum>p\<in>pts. winding_number g p * contour_integral (circlepath p (h p)) f)"
    using Cauchy_theorem_singularities[OF assms avoid] .
  also have "\<dots> = (\<Sum>p\<in>pts.  c * winding_number g p * residue f p)"
    proof (intro sum.cong)
      show "pts = pts" by simp
    next
      fix x assume "x \<in> pts"
      show "winding_number g x * contour_integral (circlepath x (h x)) f
          = c * winding_number g x * residue f x"
        proof (cases "x\<in>S")
          case False
          then have "winding_number g x=0" using homo by auto
          thus ?thesis by auto
        next
          case True
          have "contour_integral (circlepath x (h x)) f = c* residue f x"
            using \<open>x\<in>pts\<close> \<open>finite pts\<close> avoid[rule_format, OF True]
            apply (intro base_residue[of "S-(pts-{x})",THEN contour_integral_unique,folded c_def])
            by (auto intro:holomorphic_on_subset[OF holo] open_Diff[OF \<open>open S\<close> finite_imp_closed])
          then show ?thesis by auto
        qed
    qed
  also have "\<dots> = c * (\<Sum>p\<in>pts. winding_number g p * residue f p)"
    by (simp add: sum_distrib_left algebra_simps)
  finally show ?thesis unfolding c_def .
qed

(* https://github.com/seL4/isabelle/blob/19f93d7b5d76559e76a0ae8dc41f3f16a1816b5e/src/HOL/Complex_Analysis/Residue_Theorem.thy#L834C1-L846C4 *)
corollary fps_coeff_residues_bigo':
  fixes f :: "complex \<Rightarrow> complex" and r :: real
  assumes exp: "f has_fps_expansion F"
  assumes "open A" "connected A" "cball 0 r \<subseteq> A" "r > 0" 
  assumes "f holomorphic_on A - S" "S \<subseteq> ball 0 r" "finite S" "0 \<notin> S"
  assumes "eventually (\<lambda>n. g n = -(\<Sum>z \<in> S. residue (\<lambda>z. f z / z ^ Suc n) z)) sequentially"
             (is "eventually (\<lambda>n. _ = -?g' n) _")
  shows   "(\<lambda>n. fps_nth F n - g n) \<in> O(\<lambda>n. 1 / r ^ n)" (is "(\<lambda>n. ?c n - _) \<in> O(_)")
proof -
  have "fps_nth F = (\<lambda>n. (deriv ^^ n) f 0 / fact n)"
    using fps_nth_fps_expansion[OF exp] by (intro ext) simp_all
  with fps_coeff_residues_bigo[OF assms(2-)] show ?thesis by simp
qed

end