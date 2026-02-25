# `architecture.md` — Definitive Mathematical Reference for the Market Analysis + Portfolio Pipeline

This document is the **final consolidated mathematical architecture** for the pipeline we will build.
It is a **stateful, causal, end-of-day model engine** that outputs forecasts, uncertainty, structural diagnostics, risk covariance, and a tradable portfolio target.

It preserves the core design decisions already established:

* **Residual covariance uses Glasso only**
* **Risk precision / partial correlations / graph operators are distinct objects**
* **Graph is a mathematical operator layer, not just a feature source**
* **Liquidity/activity is explicit in features, uncertainty, confidence, and optimization**
* **Neighborhood mean-reversion risk is a required forecast component**
* **Clusters are retained as structural context infrastructure**
* **No hard cardinality constraint in the core optimizer** (sparsity/cleanup handled post-optimization)

---

## 1) Scope and design principles

### 1.1 What this architecture covers

This document formalizes:

1. **Data inputs and causal timing**
2. **Recursive state updates**
3. **Risk engine** (PCA + factor covariance + residual Glasso)
4. **Structure engine / graph scaffold**
5. **Signals and feature assembly**
6. **Forecast engine** (mean + uncertainty + gating/confidence/reliability)
7. **Portfolio construction** (continuous optimizer + post-optimization support shaping)
8. **Output contracts**

### 1.2 What this architecture does not fix

This document does **not** hard-code:

* specific hyperparameter values
* exact training window lengths
* solver library choice (ROI, quadprog, OSQP, Gurobi, etc.)
* execution/slippage backtest implementation details

Those are implementation/config choices that consume this mathematical spec.

---

## 2) Time, causality, and tradability conventions

## 2.1 Time index and horizon

Let ( t = 1,\dots,T ) index trading days.

Let ( \mathcal U_t ) be the universe available at decision time (t), with size ( p_t = |\mathcal U_t| ).

Fix a forecast horizon ( H \in \mathbb N ) for one model run.

For asset (i\in\mathcal U_t), define daily log return
[
r_{i,t} = \log C_{i,t} - \log C_{i,t-1}.
]

Define the (H)-day forward target
[
y^{(H)}*{i,t} = \sum*{h=1}^{H} r_{i,t+h}
= \log C_{i,t+H} - \log C_{i,t}.
]

---

## 2.2 Causality / execution timing (mandatory)

At date (t):

* ((O,H,L,C,V^{BRL},V^{units},N^{trades})_{i,t}) are assumed **fully observed after market close**.
* All state updates and computations at (t) (risk, graph, features, forecasts, optimization) are computed **after close**.
* The resulting target weights (w_t^{target}) are executable **no earlier than (t+1)** under the execution model.

This is an **EOD signal → next-day execution** architecture.

---

## 2.3 Tradability / admissibility mask

Define a binary tradability/activity mask (a_{i,t}\in{0,1}), where (a_{i,t}=1) only if minimum data-quality and trading activity conditions hold (policy-configurable, e.g.):

* valid adjusted close and return history
* (V^{BRL}_{i,t} > 0)
* (N^{trades}_{i,t} > 0)
* not halted/suspended (if such metadata exists)
* not stale beyond tolerance window (optional)

Define the optimization admissible universe:
[
\mathcal U_t^{adm}
==================

{i\in\mathcal U_t : a_{i,t}=1}
\cup
{i : w^{prev}*{i,t} > \epsilon*{hold}}.
]

This is **not** a score-based preselection; it is a **tradability/admissibility filter**.

---

## 3) Observable panel and semantic field definitions

For each asset (i) and date (t), the observable adjusted panel includes:

### 3.1 Prices (adjusted)

[
O_{i,t},\ H_{i,t},\ L_{i,t},\ C_{i,t}.
]

### 3.2 Activity / liquidity fields (semantically corrected)

[
V^{BRL}*{i,t} \equiv \texttt{traded_value}*{i,t},
\qquad
V^{units}*{i,t} \equiv \texttt{traded_units}*{i,t},
\qquad
N^{trades}*{i,t} \equiv \texttt{n_trades}*{i,t}.
]

Compatibility aliases like `turnover` / `qty` may exist in data tables, but do **not** define core mathematics.

---

## 4) Recursive state object and universe mapping

## 4.1 Model state object

The model engine maintains a recursive state:
[
\mathcal S_t = \Big(
\sigma_t,\Sigma^f_t,S^e_t,\Theta_t,P_t,\bar P_t,M_t,
A_t,A_t^{sgn},L_t,L_t^{norm},
\text{graph diagnostics}_t,
\text{Kalman states}_t,
\text{signal scalarization states}_t,
\text{forecast calibration states}_t,
\text{component error states}_t
\Big).
]

---

## 4.2 Universe mapping operator (\Pi_t) (mandatory)

Because ( \mathcal U_t ) changes over time, recursive states must be reindexed before update.

Define a mapping operator ( \Pi_t[\cdot] ) that maps state objects from universe (\mathcal U_{t-1}) to (\mathcal U_t):

* carry values on ( \mathcal U_t \cap \mathcal U_{t-1} ),
* initialize new names with configured priors,
* drop departed names.

For a symmetric matrix state (X_{t-1}):
[
[\Pi_t(X_{t-1})]_{ij}
=====================

\begin{cases}
(X_{t-1})*{\phi*{t-1}(i),\phi_{t-1}(j)}, & i,j\in \mathcal U_t\cap \mathcal U_{t-1},\
x^{init}*{diag}, & i=j,\ i\in \mathcal U_t\setminus\mathcal U*{t-1},\
0, & \text{otherwise},
\end{cases}
]
where (\phi_t) is the current index map from asset identifiers to matrix row/column indices.

Apply (\Pi_t) before recursion updates for:

* (S^e), (\bar P), (M)
* edge/node stability states
* per-asset EWMA vol states
* Kalman states
* component error states
* scalarization/calibration states if asset-specific

---

# 5) Risk engine (optimizer covariance)

This branch outputs the covariance used in portfolio risk penalization.

## 5.1 EWMA volatility state (diagonal scale)

For each asset:
[
\sigma^2_{i,t}
==============

\lambda_\sigma \sigma^2_{i,t-1}
+
(1-\lambda_\sigma) r_{i,t}^2,
\qquad 0<\lambda_\sigma<1.
]

Let
[
D_t = \mathrm{diag}(\sigma_{1,t},\dots,\sigma_{p_t,t}).
]

Define standardized returns:
[
\tilde r_t = D_t^{-1} r_t \in \mathbb R^{p_t}.
]

---

## 5.2 PCA systematic layer (on standardized returns)

On a rolling window of standardized returns
[
\widetilde R_t = [\tilde r_{t-L+1},\dots,\tilde r_t]^\top \in \mathbb R^{L\times p_t},
]
compute rank-(k) PCA to obtain raw outputs:

* loadings (B_t^{raw}\in\mathbb R^{p_t\times k})
* factor returns (f_t^{raw}\in\mathbb R^k)

---

## 5.3 PCA factor identity alignment (mandatory)

PCA factors are not time-identity-stable (sign flips, order swaps). Before any recursive factor update, align current factors to prior basis.

Let
[
C_t = B_{t-1}^\top B_t^{raw} \in \mathbb R^{k\times k}.
]

Construct a signed permutation alignment matrix (R_t \in \mathcal P_k^\pm) (Hungarian matching on (|C_t|), plus sign correction), and set:
[
B_t = B_t^{raw} R_t,
\qquad
f_t = R_t^\top f_t^{raw}.
]

All factor-recursive quantities use aligned ((B_t, f_t)).

---

## 5.4 Factor covariance recursion

[
\Sigma_t^f
==========

\lambda_f \Sigma_{t-1}^f
+
(1-\lambda_f) f_t f_t^\top.
]

(Any DCC-style factor covariance can occupy the same object slot (\Sigma_t^f), but the interface is unchanged.)

---

## 5.5 Residual branch (Glasso-only)

Residual standardized returns:
[
e_t = \tilde r_t - B_t f_t.
]

Residual covariance target (EWMA):
[
S_t^e
=====

\lambda_e S_{t-1}^e
+
(1-\lambda_e)e_t e_t^\top.
]

Residual precision via Graphical Lasso:
[
\Theta_t
========

\arg\min_{\Theta\succ 0}
\left{
-\log\det\Theta
+\mathrm{tr}(S_t^e\Theta)
+\lambda_{\Theta,t}|\Theta|_{1,\mathrm{off}}
\right}.
]

Residual covariance:
[
\Sigma_t^e = \Theta_t^{-1}.
]

---

## 5.6 Daily risk covariance recomposition

Standardized daily covariance:
[
\widetilde\Sigma_t^{risk,(1)}
=============================

B_t \Sigma_t^f B_t^\top + \Sigma_t^e.
]

Unstandardized daily covariance:
[
\Sigma_t^{risk,(1)}
===================

D_t \widetilde\Sigma_t^{risk,(1)} D_t.
]

### Numerical repair (required)

Apply:

1. symmetry repair
   [
   \Sigma_t^{risk,(1)} \leftarrow \frac{1}{2}\left(\Sigma_t^{risk,(1)} + \Sigma_t^{risk,(1)\top}\right)
   ]
2. PSD repair / eigenvalue flooring if needed

---

## 5.7 Horizon-consistent covariance (mandatory for optimization)

The forecast targets (H)-day returns, so risk penalization must use an (H)-horizon covariance.

Define:
[
\Sigma_t^{risk,(H)}
===================

H\Sigma_t^{risk,(1)}
+
\sum_{h=1}^{H-1}(H-h)\left(\Gamma_{t,h}+\Gamma_{t,h}^\top\right),
]
where
[
\Gamma_{t,h} = \mathrm{Cov}(r_{t+1}, r_{t+1+h} \mid \mathcal F_t).
]

### Default operational approximation

If lagged autocovariances are not modeled:
[
\Gamma_{t,h}\equiv 0
\quad\Rightarrow\quad
\Sigma_t^{risk,(H)} \approx H\Sigma_t^{risk,(1)}.
]

---

# 6) Structure engine (graph scaffold from residual precision)

This branch is derived from (\Theta_t) but produces **graph operators**, not covariance.

## 6.1 Partial-correlation matrix from Glasso precision

[
(P_t)_{ij}
==========

\begin{cases}
-\dfrac{(\Theta_t)*{ij}}{\sqrt{(\Theta_t)*{ii}(\Theta_t)_{jj}}}, & i\neq j,[6pt]
0, & i=j.
\end{cases}
]

This is the snapshot structural dependence object.

---

## 6.2 Adaptive temporal smoothing of partial correlations (symmetry-preserving)

Define pairwise innovation:
[
\xi_{ij,t} = |(P_t)*{ij} - (\bar P*{t-1})_{ij}|.
]

Define structural shock:
[
\chi_t = \frac{|P_t-P_{t-1}|_F}{\sqrt{p_t(p_t-1)}}.
]

Define edge stability state:
[
s^{edge}_{ij,t}
===============

\lambda_s s^{edge}*{ij,t-1}
+
(1-\lambda_s)
\mathbf 1!\left[\mathrm{sign}(P*{ij,t})=\mathrm{sign}(\bar P_{ij,t-1})\right].
]

Define pairwise gain covariates (for (i<j)):
[
z^\alpha_{ij,t} = \big[1,\ s^{edge}*{ij,t-1},\ \chi_t,\ \xi*{ij,t}\big]^\top.
]

Adaptive smoothing gain:
[
\alpha_{ij,t} = \sigma\big(\theta_\alpha^\top z^\alpha_{ij,t}\big),\qquad i<j,
]
with symmetry enforced:
[
\alpha_{ji,t}=\alpha_{ij,t},\qquad \alpha_{ii,t}=1.
]

Smoothed partial correlations:
[
(\bar P_t)_{ij}
===============

\alpha_{ij,t}(\bar P_{t-1})*{ij}
+
(1-\alpha*{ij,t})(P_t)*{ij},
\qquad i\neq j,
]
with ((\bar P_t)*{ii}=0).

---

## 6.3 Adaptive edge activation, persistence, and degree targeting

Define edge strengths:
[
u_{ij,t} = |(\bar P_t)_{ij}|,\qquad i\neq j.
]

### 6.3.1 Rowwise activation quantile

Let (r^{liq}*{i,t}) be a normalized liquidity rank proxy (e.g., rank of (\log(1+V^{BRL}*{i,t})) within universe).

Define graph density deviation:
[
\delta_t = \mathrm{dens}*{t-1} - \mathrm{dens}^{target}*{t-1}
]
(or current estimate if computed iteratively; implementation may use lagged version for causality and simplicity).

Define
[
z^{on}*{i,t} = [1,\chi_t,\delta_t,r^{liq}*{i,t}]^\top,
\qquad
q^{on}*{i,t} = \sigma(\theta*{on}^\top z^{on}*{i,t}).
]
Then
[
\tau^{on}*{i,t} = Q_{q^{on}*{i,t}}\left({u*{ij,t}:j\neq i}\right).
]

### 6.3.2 State-dependent persistence (hysteresis)

[
z^{off}*{i,t} = [1,s^{node}*{i,t-1},\chi_t,r^{liq}*{i,t}]^\top,
\qquad
\rho^{off}*{i,t} = \sigma(\theta_{off}^\top z^{off}*{i,t}),
]
[
\tau^{off}*{i,t} = \rho^{off}*{i,t}\tau^{on}*{i,t}.
]

Preliminary persistence-aware mask:
[
\widetilde M_{ij,t}
===================

\mathbf 1[u_{ij,t}\ge \tau^{on}*{i,t}]
\ \vee
\left(M*{ij,t-1}=1 \wedge u_{ij,t}\ge \tau^{off}_{i,t}\right),
\qquad i\neq j.
]

### 6.3.3 Adaptive rowwise degree target

[
z^k_{i,t} = [1,r^{liq}*{i,t},s^{node}*{i,t-1},\chi_t]^\top,
]
[
k_{i,t}
=======

k_{\min}
+
\left\lfloor
(k_{\max}-k_{\min})\sigma(\theta_k^\top z^k_{i,t})
\right\rfloor.
]

Define rowwise top-(k_{i,t}) candidate set (\mathcal N^{topk}*{i,t}) from (u*{ij,t}).

### 6.3.4 Global exception preservation

Define a global strong-edge exception set:
[
\mathcal E_t^{glob}
===================

\left{(i,j): u_{ij,t}\ge Q_{q_t^{glob}}({u_{ab,t}:a\neq b})\right},
]
where (q_t^{glob}) may be state-conditioned (e.g., on (\chi_t,\delta_t)).

Construct directed candidate mask:
[
\widehat M_{ij,t}
=================

\widetilde M_{ij,t}\cdot
\mathbf 1!\left[
j\in\mathcal N^{topk}_{i,t}
\ \vee
(i,j)\in \mathcal E_t^{glob}
\right].
]

---

## 6.4 Symmetric graph construction (post-symmetry semantics explicit)

### 6.4.1 Union symmetrization

[
M_t^{(u)} = \widehat M_t \vee \widehat M_t^\top,
\qquad
\mathrm{diag}(M_t^{(u)})=0.
]

### 6.4.2 Post-symmetry control (density/degree honesty)

Because union symmetrization can inflate degree, define symmetric edge score:
[
s^{sym}*{ij,t}=u*{ij,t}\mathbf 1[M^{(u)}_{ij,t}=1],\qquad i<j.
]

Apply post-symmetry pruning to enforce configured graph complexity controls (e.g.):

* global density target band
* maximum degree soft/hard cap
* connectivity-preserving heuristics if required

Final graph mask is (M_t).

---

## 6.5 Graph weights and operators

Define weighted adjacency (absolute and signed):
[
W^{abs}*{ij,t} = M*{ij,t}u_{ij,t}^{\beta_w},
\qquad
W^{sgn}*{ij,t} = M*{ij,t}(\bar P_t)*{ij}u*{ij,t}^{\beta_w-1},
]
with zeros on the diagonal.

Let
[
D_t^g = \mathrm{diag}\left(\sum_j W^{abs}_{ij,t}\right).
]

### Unsigned row-stochastic operator

[
A_t = (D_t^g)^{-1}W_t^{abs}
]
(with isolated-node convention handled safely in implementation).

### Signed row-stochastic operator

[
A_t^{sgn} = (D_t^g)^{-1}W_t^{sgn}.
]

### Laplacians

Unnormalized Laplacian (used for tension):
[
L_t = D_t^g - W_t^{abs}.
]

Normalized Laplacian (used for graph shrinkage regularization):
[
L_t^{norm}
==========

I - (D_t^g)^{-1/2}W_t^{abs}(D_t^g)^{-1/2}.
]

---

## 6.6 Graph diagnostics

Graph density:
[
\mathrm{dens}*t = \frac{\sum*{i<j}M_{ij,t}}{\binom{p_t}{2}}.
]

Edge turnover:
[
\mathrm{eto}_t
==============

1-\frac{\sum_{i<j}M_{ij,t}M_{ij,t-1}}{\max(1,\sum_{i<j}M_{ij,t-1})}.
]

Node stability:
[
s^{node}_{i,t}
==============

1-\frac{\sum_j |M_{ij,t}-M_{ij,t-1}|}{\max(1,\sum_j M_{ij,t-1})}.
]

Graph density deviation:
[
\delta_t = \mathrm{dens}_t - \mathrm{dens}^{target}_t.
]

---

# 7) Signal primitives (pre-graph) and scalarization

This block builds temporal and factor-based signal primitives before graph-context transforms.

## 7.1 TSMOM multiscale features

Let (\mathcal H_{mom}={h_1,\dots,h_m}) be momentum feature horizons (independent of forecast horizon (H)).

### Raw-return momentum

[
m^{raw}_{i,t}(h)
================

\frac{\sum_{u=t-h+1}^{t} r_{i,u}}{\hat\sigma^{raw}*{i,t}(h)+\varepsilon},
\qquad h\in\mathcal H*{mom}.
]

### Residual-return momentum

Define residualized raw-unit returns:
[
r^\perp_t = D_t e_t.
]
Then
[
m^{res}_{i,t}(h)
================

\frac{\sum_{u=t-h+1}^{t} r^\perp_{i,u}}{\hat\sigma^{res}*{i,t}(h)+\varepsilon},
\qquad h\in\mathcal H*{mom}.
]

---

## 7.2 Kalman local-linear-trend features (per asset)

Let (x_{i,t}=\log C_{i,t}). Use a local linear trend state-space model.

State:
[
\begin{bmatrix}
m^{lvl}*{i,t}\
m^{slope}*{i,t}
\end{bmatrix}
=============

\begin{bmatrix}
1 & 1\
0 & 1
\end{bmatrix}
\begin{bmatrix}
m^{lvl}*{i,t-1}\
m^{slope}*{i,t-1}
\end{bmatrix}
+\eta^{K}*{i,t},
\qquad
\eta^{K}*{i,t}\sim\mathcal N(0,Q_i).
]

Observation:
[
x_{i,t}
=======

[1\ \ 0]
\begin{bmatrix}
m^{lvl}*{i,t}\
m^{slope}*{i,t}
\end{bmatrix}
+\epsilon^K_{i,t},
\qquad
\epsilon^K_{i,t}\sim\mathcal N(0,R_i).
]

Extract Kalman features (example set):
[
k^{(1)}*{i,t}=\widehat m^{slope}*{i,t},
\quad
k^{(2)}*{i,t}=\mathrm{Var}(\widehat m^{slope}*{i,t}),
\quad
k^{(3)}*{i,t}=\nu^K*{i,t},
\quad
k^{(4)}*{i,t}=S^K*{i,t},
\quad
k^{(5)}*{i,t}=\frac{\nu^K*{i,t}}{\sqrt{S^K_{i,t}}}.
]

---

## 7.3 PCA factor trend features and factor-projected continuation

Let (\mathcal H_{fac}) be factor-trend feature horizons.

For factor (q=1,\dots,k):
[
g_{q,t}(h)
==========

\frac{\sum_{u=t-h+1}^{t} f_{q,u}}{\hat\sigma^f_{q,t}(h)+\varepsilon},
\qquad h\in\mathcal H_{fac}.
]

Define asset factor-loading row vector:
[
b^{fac}*{i,t} = (B_t)*{i,\cdot}\in\mathbb R^k.
]

Let (\omega^{fac}*q) map multiscale factor trend vector for factor (q) to a scalar. Then define factor-projected continuation:
[
\phi^{facproj}*{i,t}
====================

\sum_{q=1}^{k}
b^{fac}_{i,t,q}, (\omega^{fac}*q)^\top g*{q,t}.
]

---

## 7.4 Scalarization of signal families (mandatory)

For each signal family (q\in{\text{mom},\text{kal},\text{fac}}), let the raw family feature vector be
[
u^{(q)}_{i,t}\in\mathbb R^{d_q}.
]

Standardize it to (\widetilde u^{(q)}*{i,t}), then define scalar signal:
[
s^{(q)}*{i,t} = (\omega_t^{(q)})^\top \widetilde u^{(q)}_{i,t}.
]

### Recursive scalarization weights

Weights are updated by smoothed rolling ridge estimates:
[
\omega_t^{(q)}
==============

\lambda_\omega \omega_{t-1}^{(q)}
+
(1-\lambda_\omega),\omega^{(q)}_{t,\mathrm{ridge}}.
]

This keeps scalarization adaptive but stable.

---

# 8) Liquidity / activity feature block (central and explicit)

For each ((i,t)), define:

[
\ell^{(1)}*{i,t} = \log(1+V^{BRL}*{i,t}),
\qquad
\ell^{(2)}*{i,t} = \log(1+V^{units}*{i,t}),
\qquad
\ell^{(3)}*{i,t} = \log(1+N^{trades}*{i,t}).
]

Average trade sizes:
[
\ell^{(4)}*{i,t} = \frac{V^{BRL}*{i,t}}{N^{trades}*{i,t}+\varepsilon},
\qquad
\ell^{(5)}*{i,t} = \frac{V^{units}*{i,t}}{N^{trades}*{i,t}+\varepsilon}.
]

Implied average price proxy:
[
\ell^{(6)}*{i,t} = \frac{V^{BRL}*{i,t}}{V^{units}_{i,t}+\varepsilon}.
]

Amihud-like illiquidity:
[
\ell^{(7)}*{i,t} = \frac{|r*{i,t}|}{V^{BRL}_{i,t}+\varepsilon}.
]

### Rolling standardized versions

[
\widetilde\ell^{(a)}_{i,t}
==========================

\frac{\ell^{(a)}*{i,t} - \mathrm{EMA}(\ell^{(a)}*{i,\cdot})*t}
{\mathrm{EWMA_sd}(\ell^{(a)}*{i,\cdot})_t+\varepsilon}.
]

Cross-sectional ranks and z-scores may also be included as deterministic transforms.

### Missing / non-tradable semantics

If (a_{i,t}=0), feature computation uses a consistent masked/default policy with indicator flags. Non-tradability is enforced in portfolio construction (Section 13).

---

# 9) Graph operator layer (formal signal transforms)

For any scalar signal vector (s_t\in\mathbb R^{p_t}), the graph layer provides deterministic operators.

## 9.1 Peer-context transform

[
\mathcal G_t^{peer}[s_t] = A_t s_t.
]

## 9.2 Relative/dislocation transform

[
\mathcal G_t^{rel}[s_t] = s_t - A_t s_t.
]

## 9.3 Signed-neighborhood transform

[
\mathcal G_t^{sgn}[s_t] = A_t^{sgn} s_t.
]

## 9.4 Tension / local incompatibility

[
\mathcal G_t^{ten}[s_t] = L_t s_t.
]

## 9.5 Graph-regularized shrinkage transform

[
\mathcal G_{t,\lambda_g}^{shr}[s_t]
===================================

(I+\lambda_g L_t^{norm})^{-1}s_t.
]

## 9.6 One-step mixed propagation

[
\mathcal G_{t,\nu_t}^{mix}[s_t]
===============================

\big((1-\nu_t)I+\nu_t A_t\big)s_t,
\qquad \nu_t\in[0,1].
]

No iterative diffusion-to-convergence is part of the core pipeline.

---

# 10) Clustering layer (retained as structural context infrastructure)

Clusters are **derived from the graph** and used for **contextualization**, not as the primary risk engine.

## 10.1 Cluster construction (fixed method)

Use weighted spectral clustering on (L_t^{norm}) (or equivalently on (W_t^{abs}) through the normalized Laplacian).

Let cluster labels be (c(i,t)\in{1,\dots,K_t^{clust}}).

The number of clusters (K_t^{clust}) is chosen by a bounded rule (e.g., eigengap within a configured interval ([K_{min}^{clust},K_{max}^{clust}])).

---

## 10.2 Cluster label persistence across time (mandatory)

Cluster labels are permutation-ambiguous over time. To stabilize cluster-derived features, relabel clusters at (t) to maximize overlap with (t-1) clusters (Hungarian matching on intersection counts).

---

## 10.3 Cluster transforms of signals

For any signal (s_t), define cluster-centered signal:
[
\mathcal G_t^{clust_ctr}[s_t]_i
===============================

## s_{i,t}

\frac{1}{|\mathcal C_{c(i,t),t}|}\sum_{j\in\mathcal C_{c(i,t),t}} s_{j,t}.
]

Cluster-z score:
[
\mathcal G_t^{clust_z}[s_t]_i
=============================

\frac{\mathcal G_t^{clust_ctr}[s_t]*i}
{\mathrm{sd}{s*{j,t}:j\in\mathcal C_{c(i,t),t}}+\varepsilon}.
]

These are key for peer-relative dislocation and neighborhood mean-reversion features.

---

# 11) Feature engine assembly (X_{i,t})

For each asset (i), the feature engine outputs a causal feature vector (X_{i,t}).

## 11.1 Temporal trend block

Includes:

* multiscale raw TSMOM features (m^{raw}_{i,t}(h))
* multiscale residual TSMOM features (m^{res}_{i,t}(h))
* Kalman features (k^{(a)}_{i,t})
* factor-projected continuation (\phi^{facproj}_{i,t})

---

## 11.2 Structural context block (graph transforms of scalarized signals)

Let
[
s_t^{mom},\ s_t^{kal},\ s_t^{fac}\in\mathbb R^{p_t}
]
be scalarized signal vectors for momentum, Kalman, and factor families.

Include, for each family as appropriate:

* (\mathcal G_t^{peer}[s]_i)
* (\mathcal G_t^{rel}[s]_i)
* (\mathcal G_{t,\lambda_g}^{shr}[s]_i)
* (\mathcal G_t^{ten}[s]_i)
* (\mathcal G_t^{clust_z}[s]_i)

---

## 11.3 PCA–graph interaction block

Define cluster-level factor-loading centroids:
[
\bar b^{fac}_{c,t}
==================

\frac{1}{|\mathcal C_{c,t}|}\sum_{j\in\mathcal C_{c,t}} b^{fac}_{j,t}.
]

Let (\bar g_t\in\mathbb R^k) be factor-trend summaries (one scalar per factor).

Define:
[
\phi^{cf1}_{i,t}
================

b^{fac}*{i,t}{}^\top \bar g_t,
]
[
\phi^{cf2}*{i,t}
================

\big(b^{fac}*{i,t}-\bar b^{fac}*{c(i,t),t}\big)^\top \bar g_t.
]

These encode:

* absolute factor-trend exposure
* cluster-relative factor-exposure dislocation

---

## 11.4 Liquidity/activity block

Include:

* (\ell^{(a)}_{i,t})
* (\widetilde\ell^{(a)}_{i,t})
* ranks/z-scores
* interactions with signal amplitudes, e.g.
  [
  \phi^{liq\times mom}*{i,t}
  =
  |s^{mom}*{i,t}|\cdot \widetilde\ell^{(7)}*{i,t},
  \qquad
  \phi^{liq\times kal}*{i,t}
  =
  |s^{kal}*{i,t}|\cdot \widetilde\ell^{(7)}*{i,t}.
  ]

---

## 11.5 Global/structural state-context block

Global state variables (replicated or joined appropriately per asset) include:

### 11.5.1 Cross-sectional dispersion

[
\mathrm{disp}_t = \mathrm{sd}*i(r*{i,t}).
]

### 11.5.2 Market-mode dominance (one-factor dominance proxy)

Let (\lambda_{1,t}\ge \cdots \ge \lambda_{p_t,t}) be eigenvalues of the rolling correlation matrix of returns. Define:
[
\eta^{mode}_t
=============

# \frac{\lambda_{1,t}}{\sum_{j=1}^{p_t}\lambda_{j,t}}

\frac{\lambda_{1,t}}{p_t}.
]

### 11.5.3 Vol-of-vol proxy

Let
[
\bar\sigma_t = \mathrm{median}*i(\sigma*{i,t}).
]
Define:
[
\mathrm{VoV}_t
==============

\left(
\mathrm{EWMA}*{\lambda*{vov}}\big((\Delta \log \bar\sigma_t)^2\big)
\right)^{1/2}.
]

### 11.5.4 Global liquidity state

[
m_t^{liq,glob}
==============

\Big[
\mathrm{median}*i(\log(1+V^{BRL}*{i,t})),
\ \mathrm{median}*i(\log(1+N^{trades}*{i,t})),
\ \frac{1}{p_t}\sum_i \mathbf 1[V^{BRL}_{i,t}>0]
\Big]^\top.
]

### 11.5.5 Final global state vector

After robust standardization of entries,
[
m_t
===

\big[
\mathrm{disp}_t,\ \eta^{mode}_t,\ \mathrm{VoV}_t,\ \mathrm{dens}_t,\ \mathrm{eto}_t,\ \chi_t,\ (m_t^{liq,glob})^\top,\dots
\big]^\top.
]

Asset-local structural diagnostics also enter (X_{i,t}), including:

* (s^{node}_{i,t})
* degree
* neighbor liquidity summaries
* local disagreement/tension metrics

---

# 12) Forecast engine (mean + uncertainty + gating/confidence/reliability)

The forecast engine outputs, for each asset (i):

* forecast mean (\hat\mu^{(H)}_{i,t})
* forecast uncertainty (\hat s^{(H)}_{i,t})
* reliability-adjusted moments for optimization ((\hat\mu^{eff}*{i,t}, \hat s^{eff}*{i,t}))

---

## 12.1 Component forecast structure (core set)

The architecture includes the following component forecasts:

1. **Temporal continuation**
2. **Structural continuation / peer confirmation**
3. **Neighborhood mean-reversion risk** (required)
4. **PCA–graph structural dislocation**
5. **Liquidity/friction-conditioned correction**

For each component (c=1,\dots,C):
[
\mu^{(c)}*{i,t} = x^{(c)\top}*{i,t}\beta_t^{(c)},
]
where (x^{(c)}*{i,t}\subset X*{i,t}) is the component-specific feature subset.

---

## 12.2 Component definitions (semantic roles)

### Component 1: Temporal continuation

Uses:

* multiscale raw TSMOM
* multiscale residual TSMOM
* Kalman features
* factor-projected continuation
* liquidity interactions

Goal: continuation/trend signal in raw/residual/factor domains.

### Component 2: Structural continuation / peer confirmation

Uses graph transforms of trend/factor scalar signals, e.g.:

* (\mathcal G^{peer}[s^{mom}])
* (\mathcal G^{peer}[s^{kal}])
* (\mathcal G^{shr}[s^{mom}])
* cluster-z transforms
* graph stability interactions

Goal: continuation signal conditioned by structural neighborhood support.

### Component 3: Neighborhood mean-reversion risk (required)

Define dislocation features:
[
d^{mom}_{i,t} = \mathcal G_t^{rel}[s_t^{mom}]*i,
\quad
d^{kal}*{i,t} = \mathcal G_t^{rel}[s_t^{kal}]*i,
\quad
d^{fac}*{i,t} = \mathcal G_t^{rel}[s_t^{fac}]_i.
]

Define tension features:
[
\tau^{mom}_{i,t} = \mathcal G_t^{ten}[s_t^{mom}]*i,
\quad
\tau^{kal}*{i,t} = \mathcal G_t^{ten}[s_t^{kal}]_i.
]

Goal: encode overextension / reversion risk relative to structural peers.

### Component 4: PCA–graph structural dislocation

Uses:

* (\phi^{cf2}_{i,t}) (cluster-relative factor exposure dislocation)
* factor trend summaries
* cluster stability metrics
* factor–liquidity interactions

Goal: exploit PCA exposure geometry inside graph neighborhoods.

### Component 5: Liquidity/friction-conditioned correction

Uses:

* illiquidity/fragmentation features
* liquidity-scaled signal amplitudes
* local liquidity mismatch vs neighbors
* recent microstructure stress proxies

Goal: discount or correct expected returns under friction/fragility conditions.

---

## 12.3 Component model calibration (rolling regularized estimation)

Each component is fit with rolling (or expanding) delayed-label regression, using only matured labels:
[
\mathcal T_t^{train} \subseteq {s:\ s\le t-H}.
]

Define exponentially weighted ridge regression:
[
\hat\beta_t^{(c)}
=================

\arg\min_{\beta}
\sum_{s\in\mathcal T_t^{train}}
w_{t,s}
\left(y^{(H)}*{i,s} - x^{(c)\top}*{i,s}\beta\right)^2
+
\lambda_{\beta,c}|\beta|_2^2.
]

Implementation recommendations (part of the architecture contract):

* blockwise penalties (stronger on graph-transform clones)
* robust standardization of component features
* refit schedule may be slower than daily if needed, but inference remains daily

---

## 12.4 Global component mixture weights (\pi_t) (softmax gating)

Let (m_t\in\mathbb R^{d_m}) be the global state vector.

Define logits:
[
a_t = A_\pi m_t + b_\pi \in \mathbb R^C.
]

Softmax weights:
[
\pi_{c,t} = \frac{\exp(a_{c,t})}{\sum_{d=1}^{C}\exp(a_{d,t})}.
]

These are **global** component mixture weights (same across assets at time (t)).

---

## 12.5 Asset/component confidence multipliers (\kappa_{i,c,t}) (constrained)

To retain component-specific confidence without identifiability blowups, use bounded multiplicative confidence multipliers.

Raw confidence score:
[
\widetilde\kappa_{i,c,t}
========================

\sigma!\left((\alpha^{(c)})^\top \psi^{(c)}*{i,t}\right)\in(0,1),
]
where (\psi^{(c)}*{i,t}) may include:

* component uncertainty proxy
* liquidity/activity features
* node stability
* local disagreement/tension
* graph edge-turnover interactions
* component diagnostics

Bounded multiplier around 1:
[
\kappa_{i,c,t}
==============

\kappa_{min}
+
(\kappa_{max}-\kappa_{min})\widetilde\kappa_{i,c,t},
\qquad 0<\kappa_{min}<1<\kappa_{max}.
]

Define effective normalized asset/component mixture weights:
[
\widetilde q_{i,c,t} = \pi_{c,t}\kappa_{i,c,t},
\qquad
q_{i,c,t}
=========

\frac{\widetilde q_{i,c,t}}{\sum_{d=1}^{C}\widetilde q_{i,d,t}}.
]

### Final forecast mean

[
\hat\mu^{(H)}_{i,t}
===================

\sum_{c=1}^{C} q_{i,c,t}\mu^{(c)}_{i,t}.
]

This preserves both global regime gating and local confidence modulation while fixing scale identifiability.

---

## 12.6 Component error states and overlap-aware uncertainty (mandatory for (H>1))

### 12.6.1 Delayed component forecast residuals

When labels mature (for (s=t-H)):
[
\varepsilon^{(c)}_{i,s}
=======================

y^{(H)}*{i,s} - \mu^{(c)}*{i,s}.
]

### 12.6.2 Stagger-bucket error states (overlap-aware)

Because (H)-horizon labels overlap for adjacent (s), maintain (H) error-scale buckets per component (asset-specific or hierarchical pooled), indexed by (b\in{0,\dots,H-1}).

Bucket assignment:
[
b(s)= s \bmod H.
]

Update only the matching bucket:
[
(\hat s^{(c,b)}_{i,t})^2
========================

\lambda^{(c)}*{err}(\hat s^{(c,b)}*{i,t-1})^2
+
(1-\lambda^{(c)}*{err})(\varepsilon^{(c)}*{i,s})^2,
\qquad b=b(s).
]

Other buckets carry forward unchanged.

Aggregate component error scale:
[
(\hat s^{(c)}_{i,t})^2
======================

\mathrm{median}*{b=0,\dots,H-1}(\hat s^{(c,b)}*{i,t})^2.
]

---

## 12.7 Component forecast-error covariance (\Omega^\mu_{i,t})

Let (e^{comp}_{i,t}\in\mathbb R^C) be the vector of matured component residuals for asset (i) (or pooled residuals by bucket/regime, depending on estimation granularity).

Maintain a component forecast-error covariance estimate:
[
\Omega^\mu_{i,t}\in\mathbb R^{C\times C},
]
with diagonal-only or shrinkage-to-diagonal initialization allowed operationally.

Combined forecast uncertainty:
[
(\hat s^{(H)}_{i,t})^2
======================

q_{i,t}^\top \Omega^\mu_{i,t} q_{i,t}.
]

---

## 12.8 Asset-level reliability score (optimizer-facing contraction)

Define an asset-level reliability score:
[
\rho^{rel}_{i,t}
================

\sigma!\left((\alpha^{rel})^\top \psi^{rel}_{i,t}\right)\in(0,1),
]
with features including:

* liquidity stress / fragmentation
* graph instability / edge turnover exposure
* local disagreement/tension
* forecast uncertainty diagnostics

Define optimizer-facing effective moments:
[
\hat\mu^{eff}_{i,t}
===================

\rho^{rel}*{i,t}\hat\mu^{(H)}*{i,t},
]
[
(\hat s^{eff}_{i,t})^2
======================

\frac{(\hat s^{(H)}*{i,t})^2}{\rho^{rel}*{i,t}+\varepsilon}.
]

The optimizer directly consumes (\hat\mu^{eff}) and (\Sigma^{risk,(H)}) (Section 13).
(\hat s^{eff}) is available for diagnostics and post-processing policies.

---

# 13) State-conditioned optimizer controls

Map global state (m_t) to optimizer control scalars.

Risk aversion:
[
\gamma_t = \exp(\theta_\gamma^\top m_t).
]

Turnover penalty scale:
[
\tau_t = \exp(\theta_\tau^\top m_t).
]

Entry/exit count penalties are **not part of the core optimizer** in this architecture (see Section 14). If needed, analogous state maps can be retained for post-optimization support shaping policies.

Risky gross exposure cap:
[
\rho_t^{gross} = \sigma(\theta_{gross}^\top m_t)\in(0,1).
]

Additional control maps (e.g., risk budgets by cluster/factor, turnover budgets, max tracking aggressiveness) may be defined as deterministic functions of (m_t) and diagnostics.

---

# 14) Portfolio construction (continuous optimizer + post-optimization support shaping)

This architecture **does not enforce hard cardinality** in the core optimizer.

Selection and optimization are still economically integrated through expected returns, risk, turnover penalties, liquidity caps, and tradability constraints. Exact sparsity/count shaping is handled in a dedicated post-optimization stage.

---

## 14.1 Core optimization problem (continuous, long-only)

Decision variables on ( \mathcal U_t^{adm} ):

* (w_i \ge 0): risky asset weights
* (d_i \ge 0): turnover auxiliaries
* (w_{cash}\ge 0) (if explicit cash sleeve is modeled)

Given:

* (\hat\mu_t^{eff} = (\hat\mu^{eff}*{1,t},\dots,\hat\mu^{eff}*{p_t,t})^\top)
* (\Sigma_t^{risk,(H)})
* previous weights (w^{prev}_t)
* liquidity-aware caps/cost coefficients
* tradability mask (a_{i,t})
* control scalars ((\gamma_t,\tau_t,\rho_t^{gross}))

### Liquidity-aware bounds and turnover costs

Weight cap:
[
\bar w_{i,t}
============

\min!\left(\bar w^{base},\ \bar w_t^{liq}\cdot h_w(\ell_{i,t})\right),
]
where (h_w(\cdot)) is monotone in liquidity/activity quality.

Optional minimum active weight (used in post-shaping repair solve, not necessarily in the first continuous solve):
[
\underline w_{i,t}\ge 0.
]

Asset-specific turnover penalty coefficient:
[
\kappa^{turn}_{i,t}
===================

\tau_t \cdot h_{turn}(\ell_{i,t}),
]
with (h_{turn}(\cdot)) increasing in illiquidity/fragmentation.

### Core objective (maximize)

[
\max_{w,d,w_{cash}}
\quad
(\hat\mu_t^{eff})^\top w
------------------------

## \frac{\gamma_t}{2} w^\top \Sigma_t^{risk,(H)} w

\sum_i \kappa^{turn}_{i,t} d_i.
]

Equivalent minimization form may be used in implementation.

### Constraints

Budget / risky gross:
[
\sum_i w_i = \rho_t^{gross},
\qquad
w_{cash}=1-\rho_t^{gross}
]
(or a capped-gross version if desired).

Bounds:
[
0 \le w_i \le \bar w_{i,t}.
]

Tradability:
[
w_i \le \bar w_{i,t},a_{i,t}.
]
(Equivalent to solving only on ( \mathcal U_t^{adm} ) and handling carry positions explicitly.)

Turnover auxiliaries:
[
d_i \ge w_i - w^{prev}*{i,t},
\qquad
d_i \ge -(w_i - w^{prev}*{i,t}).
]

Additional linear constraints (if configured) may include:

* factor exposure limits (using (B_t))
* cluster concentration limits (using (c(i,t)))
* sector/class constraints
* liquidity bucket exposure limits

---

## 14.2 Tractability policy (without score-based preselection)

There is **no external top-(N)** score preselection stage.

Solver tractability is handled by:

1. **Continuous formulation** (no binary cardinality variables)
2. Warm starts (e.g., previous weights or relaxed surrogate)
3. Time limits / tolerances / regularization
4. Risk model simplifications if needed (e.g., low-rank + diagonal algebraic forms) **inside the same optimization problem**
5. Post-optimization support shaping (Section 14.3), not preselection

---

## 14.3 Post-optimization support shaping (standard stage; outside core optimizer)

This stage handles **sparsity cleanup / weight stabilization / implementability** outside the core objective.

Let (w_t^{raw}) be the continuous optimizer solution.

### 14.3.1 Thresholding

Apply a weight floor threshold:
[
\widetilde w_{i,t}
==================

w^{raw}*{i,t},\mathbf 1[w^{raw}*{i,t}\ge \tau_t^{w,drop}],
]
where (\tau_t^{w,drop}) may be fixed or state-conditioned.

### 14.3.2 Renormalization

If (\sum_i \widetilde w_{i,t}>0), renormalize to risky gross target:
[
w^{shape}_{i,t}
===============

\rho_t^{gross}
\cdot
\frac{\widetilde w_{i,t}}{\sum_j \widetilde w_{j,t}}.
]

### 14.3.3 Repair solve on fixed support (recommended)

To restore optimality after thresholding, optionally solve a **second continuous optimization** on the fixed support
[
\mathcal S_t^{supp} = {i: w^{shape}_{i,t}>0},
]
using the same objective family and constraints, with (w_i=0) for (i\notin \mathcal S_t^{supp}).

This preserves the no-cardinality core while improving final weight quality.

### 14.3.4 Optional implementation policies (outside core optimizer)

If desired, additional support-shaping logic can be applied **here** (not inside the optimizer), such as:

* minimum lot / minimum notional filters
* cap on tiny residual positions
* operational name-count soft targets
* entry/exit smoothing policies

These are implementation policies, not the mathematical core optimizer.

---

## 14.4 Final portfolio target

The output portfolio target from portfolio construction is:
[
w_t^{target},
]
which is either:

* (w_t^{raw}) (if no support shaping is applied), or
* the repaired/shaped portfolio (w_t^{shape/repair}).

This target is executed no earlier than (t+1).

---

# 15) Output artifact contract

At each rebalance time (t), the model engine outputs the object:
[
\mathcal O_t =
\Big(
\hat\mu_t^{(H)},
\hat\mu_t^{eff},
\hat s_t^{(H)},
\hat s_t^{eff},
\Sigma_t^{risk,(1)},
\Sigma_t^{risk,(H)},
\Theta_t,P_t,\bar P_t,M_t,A_t,A_t^{sgn},L_t,L_t^{norm},
m_t,\pi_t,\kappa_t,\rho_t^{rel},
\gamma_t,\tau_t,\rho_t^{gross},
\text{graph diagnostics}_t,
\text{forecast diagnostics}_t,
\text{optimizer diagnostics}_t
\Big),
]
and (if portfolio construction is enabled in this run):
[
w_t^{target}.
]

### Notes

* (\kappa_t) denotes the asset-component confidence tensor ((\kappa_{i,c,t}))
* (\rho_t^{rel}) denotes the vector ((\rho^{rel}_{i,t}))
* diagnostics must include enough metadata to audit causality, solver behavior, and state transitions

---

# 16) End-to-end update flow (single causal pass)

At each EOD date (t):

1. **Ingest panel** ((O,H,L,C,V^{BRL},V^{units},N^{trades})); compute returns (r_t)
2. Build **tradability mask** (a_{i,t})
3. Apply **universe mapping** (\Pi_t) to all recursive states
4. Update **EWMA volatility** (\sigma_t); compute standardized returns (\tilde r_t)
5. Run PCA on rolling standardized window (\widetilde R_t) (\to (B_t^{raw}, f_t^{raw}))
6. **Align PCA factors/loadings** (\to (B_t,f_t))
7. Update **factor covariance** (\Sigma_t^f)
8. Compute residuals (e_t = \tilde r_t - B_t f_t)
9. Update residual target (S_t^e); run **Glasso** (\to \Theta_t); invert (\to \Sigma_t^e)
10. Recompose and repair (\Sigma_t^{risk,(1)})
11. Build (\Sigma_t^{risk,(H)}) (default (H)-scaled if no autocov model)
12. Derive (P_t); update adaptive smoothed (\bar P_t)
13. Build adaptive graph mask (M_t) (activation + persistence + top-(k) + global exceptions + post-symmetry control)
14. Construct graph operators (A_t,A_t^{sgn},L_t,L_t^{norm}) and graph diagnostics
15. Compute signal primitives (TSMOM raw/residual, Kalman, factor trends)
16. Update signal scalarization weights (\omega_t^{(q)}); compute scalar signals (s_t^{mom}, s_t^{kal}, s_t^{fac})
17. Run clustering on (L_t^{norm}); apply label persistence
18. Assemble feature vectors (X_{i,t}) (temporal + structural + PCA–graph + liquidity + state context)
19. Update/fit component models; compute (\mu^{(c)}_{i,t})
20. Compute global softmax (\pi_t) from (m_t)
21. Compute bounded (\kappa_{i,c,t}), normalized (q_{i,c,t}), and final (\hat\mu^{(H)}_{i,t})
22. Update stagger-bucket component error states using matured labels; compute (\hat s^{(H)}_{i,t})
23. Compute (\rho^{rel}*{i,t}); derive (\hat\mu^{eff}*{i,t}, \hat s^{eff}_{i,t})
24. Map (m_t) to optimizer controls ((\gamma_t,\tau_t,\rho_t^{gross}))
25. Solve core continuous portfolio optimization (\to w_t^{raw})
26. Apply post-optimization support shaping / repair (if configured) (\to w_t^{target})
27. Emit (\mathcal O_t) and (w_t^{target}); schedule execution at (t+1)

---

# 17) Architecture invariants and implementation sanity checks (must hold)

## 17.1 Causality invariants

* No features at (t) use labels beyond (t)
* Training/calibration uses only labels (y^{(H)}_{i,s}) with (s\le t-H)
* Execution occurs no earlier than (t+1)

## 17.2 Matrix invariants

* (\Theta_t) symmetric positive definite (Glasso output)
* (\Sigma_t^{risk,(1)}), (\Sigma_t^{risk,(H)}) symmetric PSD after repair
* (M_t) symmetric with zero diagonal
* (W_t^{abs}\ge 0)
* (L_t^{norm}) numerically stable under isolated-node handling

## 17.3 Identity stability invariants

* PCA factor alignment applied before any recursive factor update
* Cluster labels matched across time before cluster-derived features are computed

## 17.4 Optimization invariants

* Final (w_t^{target}) respects tradability, bounds, and gross budget
* Post-shaping/repair does not violate hard constraints
* If repair solve fails, fallback to constraint-safe normalized shaped weights

---

# 18) Summary of architectural role of clusters (for reference)

Clusters are **retained and untouched** in this architecture as a **structural context layer**, not the primary risk engine. They are used to:

* normalize signals within structural neighborhoods (cluster-centered / cluster-z)
* define PCA–graph interaction features (factor exposure dislocation within cluster)
* optionally support diversification diagnostics/constraints

The architecture remains valid without clusters, but loses substantial context richness. In this final design, they remain part of the core feature/context infrastructure.

---

# 19) Final statement of the portfolio construction philosophy (no hard cardinality)

This architecture explicitly adopts:

* **continuous core optimization** for stability and reliability
* **no hard cardinality constraint** in the core optimizer
* **support shaping after optimization** for sparsity/implementability

This preserves the economic optimization problem while moving brittle combinatorial constraints out of the critical path.

---

If you want next, I can convert this directly into a **clean GitHub-ready `architecture.md` formatting pass** (same content, but with compact notation tables, symbol glossary, and implementation contracts per module file).
