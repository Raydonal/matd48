# Metodologia de superfície de resposta {#superficie-resposta-cap}



Os Capítulos 5 e 6 trataram fatoriais desenhados para **detectar e estimar efeitos** — a pergunta
era "quais fatores importam, e há interação?". Este capítulo muda a pergunta para "qual combinação
de níveis **otimiza** a resposta?" — a metodologia de superfície de resposta (MSR), um dos usos
mais comuns de delineamento de experimentos na indústria e na ciência de dados hoje em dia (ajuste
de hiperparâmetros, otimização de processos químicos e de manufatura, desenho de produtos).

## Introdução à superfície de resposta {#superficie-resposta}

Fatoriais $2^k$ e $3^k$ são desenhados para **detectar e estimar efeitos** — a pergunta é "quais
fatores importam, e há interação?". A **metodologia de superfície de resposta** (MSR), formalizada
por Box e Wilson [-@boxwilson1951] como uma sequência de fatoriais e desenhos aumentados que
"escala" a superfície na direção de maior melhoria [@boxhunterhunter2005; @montgomery2017design], muda a
pergunta para "qual combinação de níveis **otimiza** a resposta?", tipicamente ajustando um modelo
polinomial de segunda ordem que permite representar curvatura:

$$
y = \beta_0 + \sum_{i=1}^k \beta_i x_i + \sum_{i=1}^k \beta_{ii} x_i^2
+ \sum_{i<j} \beta_{ij} x_i x_j + \varepsilon.
$$

Os termos quadráticos $\beta_{ii}$ são o que distingue este modelo do fatorial $2^k$ (que só
estima efeitos lineares e de interação): eles permitem capturar um máximo, mínimo ou ponto de
sela dentro da região experimental, em vez de apenas uma tendência linear.

```{=html}
<div class="caixa-r"><strong>Uso do R</strong> — ajustando e visualizando a superfície de resposta
da energia de corte</div>
```


``` r
energia <- read_csv("data/energia.csv", show_col_types = FALSE)

modelo_rsm <- lm(
  energia ~ Velocidad + angulo + I(Velocidad^2) + I(angulo^2) + Velocidad:angulo,
  data = energia
)
summary(modelo_rsm)$coefficients %>%
  round(4) %>%
  kable(caption = "Modelo de segunda ordem para energia de corte") %>%
  kable_styling(full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:energia-rsm)Modelo de segunda ordem para energia de corte</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Std. Error </th>
   <th style="text-align:right;"> t value </th>
   <th style="text-align:right;"> Pr(&gt;|t|) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.0768 </td>
   <td style="text-align:right;"> 0.0479 </td>
   <td style="text-align:right;"> 1.6035 </td>
   <td style="text-align:right;"> 0.1193 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Velocidad </td>
   <td style="text-align:right;"> 0.0448 </td>
   <td style="text-align:right;"> 0.0260 </td>
   <td style="text-align:right;"> 1.7261 </td>
   <td style="text-align:right;"> 0.0946 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> angulo </td>
   <td style="text-align:right;"> -0.0041 </td>
   <td style="text-align:right;"> 0.0010 </td>
   <td style="text-align:right;"> -3.9442 </td>
   <td style="text-align:right;"> 0.0004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(Velocidad^2) </td>
   <td style="text-align:right;"> -0.0055 </td>
   <td style="text-align:right;"> 0.0037 </td>
   <td style="text-align:right;"> -1.4864 </td>
   <td style="text-align:right;"> 0.1476 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(angulo^2) </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 3.6139 </td>
   <td style="text-align:right;"> 0.0011 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Velocidad:angulo </td>
   <td style="text-align:right;"> -0.0002 </td>
   <td style="text-align:right;"> 0.0001 </td>
   <td style="text-align:right;"> -1.0642 </td>
   <td style="text-align:right;"> 0.2957 </td>
  </tr>
</tbody>
</table>

O termo quadrático de $\hat\beta_{\text{ângulo}^2}$ é altamente significativo ($p<0{,}01$), e o
$R^2$ ajustado do modelo (0.773) confirma que a
superfície de segunda ordem descreve bem os dados — evidência de curvatura real, que um fatorial
$2^k$ nesses mesmos dois fatores nunca teria detectado, porque um desenho de apenas dois níveis
por fator é algebricamente cego a termos quadráticos.

<img src="07-superficie-resposta_files/figure-html/energia-superficie-1.png" alt="" width="80%" style="display: block; margin: auto;" />

<img src="07-superficie-resposta_files/figure-html/energia-contorno-1.png" alt="" width="80%" style="display: block; margin: auto;" />

A perspectiva 3D e o gráfico de contorno são duas janelas para o mesmo objeto matemático: a
primeira dá intuição sobre a forma geral da superfície (aqui, a energia sobe em algumas direções e
desce em outras — um sinal visual de que a curvatura não é simplesmente "uma tigela"); a segunda é
mais útil para leitura precisa, e para localizar exatamente onde a superfície para de subir ou
descer.

### Classificando o ponto estacionário: gradiente e Hessiana {#ponto-estacionario}

Derivando o modelo de segunda ordem em relação a cada regressor e igualando a zero, o **ponto
estacionário** $\mathbf{x}_0$ resolve o sistema linear

$$
\nabla y(\mathbf{x}_0) = \mathbf{b} + 2\mathbf{B}\mathbf{x}_0 = \mathbf{0}
\quad\Longrightarrow\quad
\mathbf{x}_0 = -\tfrac{1}{2}\mathbf{B}^{-1}\mathbf{b},
$$

em que $\mathbf{b} = (\hat\beta_{\text{Velocidad}}, \hat\beta_{\text{ângulo}})'$ é o vetor de
coeficientes lineares e $\mathbf{B}$ é a matriz **simétrica dos coeficientes de segunda ordem**
(quadráticos na diagonal, de interação fora dela),

$$
\mathbf{B} = \begin{pmatrix} \hat\beta_{\text{Velocidad}^2} & \hat\beta_{\text{Velocidad}\cdot
\text{ângulo}}/2 \\ \hat\beta_{\text{Velocidad}\cdot\text{ângulo}}/2 & \hat\beta_{\text{ângulo}^2}
\end{pmatrix}.
$$

Note que $\mathbf{B}$ **não** é a Hessiana: do gradiente $\nabla \hat y = \mathbf{b} + 2\mathbf{B}\mathbf{x}$
segue $\nabla^2 \hat y = 2\mathbf{B}$. As duas matrizes têm os mesmos sinais de autovalores — por
isso a classificação do ponto estacionário é a mesma —, mas são os autovalores de $\mathbf{B}$, e
não os da Hessiana, que aparecem como as **taxas** $\lambda_i$ da forma canônica adiante.

A **natureza** do ponto estacionário — máximo, mínimo ou sela — é decidida pelos autovalores de
$\mathbf{B}$: ambos negativos indicam máximo, ambos positivos indicam mínimo, e **sinais opostos
indicam ponto de sela** (a superfície sobe em uma direção do espaço dos regressores e desce em
outra, exatamente o padrão sugerido pelo gráfico anterior).


``` r
b_rsm <- coef(modelo_rsm)
B_hess <- matrix(
  c(2 * b_rsm["I(Velocidad^2)"],     b_rsm["Velocidad:angulo"],
    b_rsm["Velocidad:angulo"],       2 * b_rsm["I(angulo^2)"]),
  nrow = 2
) / 2   # B tem beta_ii na diagonal e beta_ij/2 fora: o 2*.../2 da diagonal devolve
    # o proprio beta_ii, e o termo de interacao fica dividido por 2

ponto_estacionario <- -0.5 * solve(B_hess) %*% c(b_rsm["Velocidad"], b_rsm["angulo"])
rownames(ponto_estacionario) <- c("Velocidade", "Ângulo")
ponto_estacionario
```

```
##                 [,1]
## Velocidade  3.282264
## Ângulo     56.358108
```

``` r
eigen(B_hess)$values   # sinais dos autovalores classificam o ponto
```

```
## [1]  4.158071e-05 -5.510702e-03
```

Os dois autovalores têm **sinais opostos** — o ponto estacionário ($V\approx3{,}28$,
ângulo$\approx56{,}4°$) é uma **sela**, não um mínimo nem um máximo. Isso significa que, dentro da
região experimental, o menor valor de energia predito não está nesse ponto interior, mas em algum
ponto da **fronteira** da região — a varredura da grade confirma que o mínimo prático fica no
canto $V=4{,}5$, ângulo$\approx59{,}0°$ (energia $\approx0{,}027$). Esse é exatamente o tipo de
situação em que o gráfico de contorno é indispensável: sem ele, seria fácil confundir "a superfície
tem curvatura" com "a superfície tem um ótimo interior", quando na verdade tem as duas coisas ao
mesmo tempo, mas não no mesmo ponto.

### Análise canônica: além de classificar, descrever a forma da superfície {#analise-canonica}

A classificação do ponto estacionário acima (máximo, mínimo ou sela) usa só os **sinais** dos
autovalores de $\mathbf{B}$. A **análise canônica** usa os autovalores *e* autovetores completos
para reescrever o modelo ajustado numa base em que a curvatura fica diagonal — revelando não só
*que tipo* de ponto estacionário existe, mas *quão rápido* a resposta muda em cada direção
principal da superfície. Partindo do modelo centrado no ponto estacionário $\mathbf{x}_0$,
$\hat y - \hat y_0 = (\mathbf{x}-\mathbf{x}_0)'\mathbf{B}(\mathbf{x}-\mathbf{x}_0)$, e da
decomposição espectral $\mathbf{B} = \mathbf{V}\boldsymbol\Lambda\mathbf{V}'$ ($\mathbf{V}$
ortogonal, colunas = autovetores; $\boldsymbol\Lambda=\text{diag}(\lambda_1,\lambda_2)$), a mudança
de variável $\mathbf{w} = \mathbf{V}'(\mathbf{x}-\mathbf{x}_0)$ (uma rotação de eixos, não uma
translação arbitrária) elimina o termo cruzado:
$$
\hat y = \hat y_0 + \lambda_1 w_1^2 + \lambda_2 w_2^2.
$$

```{=html}
<div class="caixa-r"><strong>Uso do R</strong> — forma canônica da superfície da energia de corte</div>
```


``` r
decomp <- eigen(B_hess)
V_rot <- decomp$vectors
lambda_canonico <- decomp$values

y0_chapeu <- predict(modelo_rsm, newdata = as.data.frame(t(ponto_estacionario)) %>%
                        setNames(c("Velocidad", "angulo")))

list(y0 = unname(y0_chapeu), lambda = lambda_canonico, eixos_w = V_rot)
```

```
## $y0
## [1] 0.03579915
## 
## $lambda
## [1]  4.158071e-05 -5.510702e-03
## 
## $eixos_w
##             [,1]        [,2]
## [1,]  0.01381628 -0.99990455
## [2,] -0.99990455 -0.01381628
```

Os eixos canônicos $w_1,w_2$ são combinações lineares (rotacionadas) de velocidade e ângulo — não
correspondem a nenhum dos dois fatores originais isoladamente. O sinal oposto de
$\lambda_1\approx$ 4.16\times 10^{-5} e $\lambda_2\approx$
-0.00551 é a mesma informação da Seção \@ref(ponto-estacionario) (sela),
mas agora quantificada. A leitura dos sinais é direta: ao longo de $w_i$ a superfície **sobe** se
$\lambda_i>0$ e **desce** se $\lambda_i<0$, a uma taxa dada por $|\lambda_i|$. Aqui
$\lambda_1>0$ e $\lambda_2<0$: a superfície sobe ao longo de $w_1$ e cai ao longo de $w_2$.

O que a análise canônica acrescenta à simples classificação do ponto estacionário é a **escala
relativa** das duas curvaturas. Como
$|\lambda_2|/|\lambda_1| \approx$ 133,
a sela é fortemente **alongada**: ao longo de $w_1$ a superfície é quase plana, enquanto ao longo
de $w_2$ ela muda duas ordens de grandeza mais rápido. É $w_2$ — a direção de **maior**
$|\lambda|$ — que domina o comportamento local da resposta, e é nela que vale a pena mover o
processo. Uma crista quase plana como a de $w_1$ é, na prática, uma faixa de combinações
velocidade/ângulo praticamente equivalentes em energia: liberdade para otimizar *outro* critério
(custo, desgaste de ferramenta) sem perda mensurável nesta resposta [@myersmontgomery2016].

### Análise de ridge: otimizando dentro de um raio fixo da região experimental {#ridge-analysis}

Dois problemas distintos tornam o ponto estacionário um alvo insuficiente. O primeiro é ele cair
**fora** da região onde os dados foram coletados: aí segui-lo é extrapolar, e o modelo de segunda
ordem só é confiável *dentro* da nuvem de pontos observados. O segundo — o caso **deste** exemplo,
em que $\mathbf{x}_0=(3.28\text{ m/s};\ 56.4^\circ)$
está confortavelmente dentro dos dados — é o ponto estacionário ser uma **sela**: ele não é nem
máximo nem mínimo, de modo que "ir até $\mathbf{x}_0$" simplesmente não responde à pergunta de
otimização.

A **análise de ridge** [@myersmontgomery2016] contorna os dois casos com uma pergunta mais modesta:
para cada raio fixo $\rho$ (distância ao centro do desenho), qual é o melhor ponto sobre o círculo
(ou esfera, em mais dimensões) de raio $\rho$? Isso é otimização restrita — otimizar
$\hat y(\mathbf{x})$ sujeito a $\mathbf{x}'\mathbf{x}=\rho^2$ —, cuja condição de
estacionariedade de Lagrange é
$$
\mathbf{b} + 2\mathbf{B}\mathbf{x} = 2\mu\,\mathbf{x}
\quad\Longleftrightarrow\quad
\mathbf{x}(\mu) = \tfrac{1}{2}(\mu\mathbf{I}-\mathbf{B})^{-1}\mathbf{b},
$$
em que o multiplicador de Lagrange $\mu$ é ajustado até que $\lVert\mathbf{x}(\mu)\rVert=\rho$.

A condição de estacionariedade sozinha **não basta**: para um mesmo $\rho$ ela admite várias
raízes $\mu$, e cada uma corresponde a um ponto crítico diferente sobre o círculo. A escolha do
ramo é o que decide qual delas é a resposta procurada [@myersmontgomery2016; @draper1963ridge]:

$$
\mu > \lambda_{\max}(\mathbf{B}) \;\Rightarrow\; \textbf{máximo restrito},
\qquad
\mu < \lambda_{\min}(\mathbf{B}) \;\Rightarrow\; \textbf{mínimo restrito},
$$

e dentro de cada ramo $\lVert\mathbf{x}(\mu)\rVert$ é monótona em $\mu$, o que torna o ajuste de
$\mu$ a um $\rho$ desejado um problema unidimensional bem posto. Fora desses dois ramos obtêm-se
pontos de sela da função restrita — críticos, mas nem máximo nem mínimo. Varrendo $\rho$ de $0$ até
a borda da região experimental, obtém-se o **caminho de ridge**: a sequência de pontos
ótimos-restritos, um para cada raio.

Velocidade e ângulo estão em escalas e unidades muito diferentes ($2{,}3$–$4{,}5\text{ m/s}$ contra
$20°$–$60°$) — um "raio" euclidiano só faz sentido depois de **codificar** as duas variáveis para
uma escala comum, a mesma convenção de $\pm1$ já usada em todo o livro para fatoriais.


``` r
c_vel <- mean(range(energia$Velocidad)); s_vel <- diff(range(energia$Velocidad)) / 2
c_ang <- mean(range(energia$angulo));    s_ang <- diff(range(energia$angulo)) / 2

energia_cod <- energia %>%
  mutate(z1 = (Velocidad - c_vel) / s_vel, z2 = (angulo - c_ang) / s_ang)

modelo_rsm_cod <- lm(energia ~ z1 + z2 + I(z1^2) + I(z2^2) + z1:z2, data = energia_cod)
b_cod <- coef(modelo_rsm_cod)[c("z1", "z2")]
B_cod <- matrix(
  c(2 * coef(modelo_rsm_cod)["I(z1^2)"], coef(modelo_rsm_cod)["z1:z2"],
    coef(modelo_rsm_cod)["z1:z2"],       2 * coef(modelo_rsm_cod)["I(z2^2)"]),
  nrow = 2
) / 2
y0_cod <- coef(modelo_rsm_cod)["(Intercept)"]
```


``` r
# Ate onde o raio pode ir sem extrapolar? A regiao experimental e a CAIXA
# [-1,1]^2 (grade 3x3 em unidades codificadas). O circulo de raio rho so fica
# inteiramente dentro da caixa ate rho = 1 (circulo INSCRITO); a partir dai ele
# escapa pelos lados, tocando o desenho apenas nos quatro vertices, que estao a
# raio sqrt(2). Paramos em 1: e o maior raio para o qual NENHUMA direcao theta
# leva para fora dos dados.
raio_max <- 1
raios <- seq(0.2, raio_max, by = 0.1)

y_no_circulo <- function(theta, rho) {
  z_theta <- rho * c(cos(theta), sin(theta))
  as.numeric(y0_cod) + as.numeric(t(z_theta) %*% b_cod) + as.numeric(t(z_theta) %*% B_cod %*% z_theta)
}

caminho_ridge <- map_dfr(raios, function(rho) {
  # y_no_circulo(theta) tem em geral DOIS mínimos e DOIS máximos em [0,2pi) (forma quadrática
  # indefinida sobre um círculo) -- optimize() assume unimodalidade e pode convergir para um
  # mínimo local errado; uma busca em grade fina, seguida de refinamento local, é mais confiável.
  grade_theta <- seq(0, 2 * pi, length.out = 721)[-721]
  th0 <- grade_theta[which.min(sapply(grade_theta, y_no_circulo, rho = rho))]
  opt <- optimize(function(th) y_no_circulo(th, rho),
                   interval = c(th0 - 2 * pi / 360, th0 + 2 * pi / 360), maximum = FALSE)
  z_theta <- rho * c(cos(opt$minimum), sin(opt$minimum))
  tibble(raio = rho, Velocidad = c_vel + s_vel * z_theta[1], angulo = c_ang + s_ang * z_theta[2],
         y_predito = opt$objective)
})

# Fronteira da regiao experimental, para deixar visivel que o caminho fica dentro dela.
fronteira <- tibble(theta = seq(0, 2 * pi, length.out = 361)) %>%
  mutate(Velocidade = c_vel + s_vel * raio_max * cos(theta),
         `Ângulo`   = c_ang + s_ang * raio_max * sin(theta))

caminho_ridge %>%
  rename(Velocidade = Velocidad, `Ângulo` = angulo) %>%
  ggplot(aes(Velocidade, `Ângulo`)) +
  geom_path(data = fronteira, linetype = "dashed", color = "grey55") +
  geom_path(aes(color = y_predito), linewidth = 1) +
  geom_point(aes(color = y_predito), size = 2) +
  scale_color_viridis_c(name = "Energia\npredita") +
  labs(title = "Caminho de ridge: melhor ponto para cada\nraio ao centro do desenho",
       subtitle = paste0("Tracejado: maior circulo inteiramente contido na regiao\nexperimental ",
                         "(raio codificado ", round(raio_max, 2), "). O caminho nao o ultrapassa."),
       x = "Velocidade de corte (m/s)", y = "Ângulo de saída (graus)") +
  theme_minimal(base_size = 12)
```

<img src="07-superficie-resposta_files/figure-html/energia-ridge-1.png" alt="" width="80%" style="display: block; margin: auto;" />

O caminho de ridge sai do centro do desenho e se afasta em direção à combinação de
velocidade/ângulo que **minimiza** a energia predita a cada raio — aqui o objetivo é minimizar,
porque energia de corte é custo; o problema de máximo é o mesmo com o sinal trocado, e corresponde
ao ramo $\mu > \lambda_{\max}(\mathbf{B})$ da condição de Lagrange. O raio só é bem definido
porque as duas variáveis foram trazidas à mesma escala.

Duas observações sobre o que a figura mostra. Primeira: o caminho **para no círculo tracejado**.
Vale explicitar por que o limite é $\rho=1$ e não $\rho=\sqrt2$. A região experimental aqui é a
*caixa* $[-1,1]^2$ — uma grade $3\times3$ —, enquanto o caminho de ridge percorre *círculos*. O
maior círculo inteiramente contido na caixa é o **inscrito**, de raio $1$; o círculo de raio
$\sqrt2\approx1{,}41$ toca o desenho apenas nos quatro vértices e, em todas as demais direções,
já está fora dos dados. Como a direção ótima $\theta$ é escolhida pelo próprio procedimento, e não
por nós, só $\rho\le1$ garante que *nenhuma* direção escolhida seja extrapolação — que é a
disciplina que esta seção prega e precisa praticar. (Em desenhos cuja região é esférica por
construção, como o CCD rotacionável da próxima seção, essa distinção desaparece: lá a região *é* um
círculo, e o raio axial $\alpha$ é o limite natural.) Segunda: em vez de relatar apenas "o mínimo
fica na fronteira", como fizemos antes, o caminho mostra *toda a trajetória* de pontos
ótimos-restritos, permitindo ao engenheiro escolher conscientemente um raio que equilibre ganho na
resposta e proximidade da região onde o modelo foi de fato estimado — quanto maior o raio, maior o
ganho predito e maior a incerteza sobre ele.

## O caminho de máxima inclinação: de um fatorial inicial até a região do ótimo {#steepest-ascent}

A energia de corte partiu de um modelo de segunda ordem já ajustado — mas, na prática, raramente se
começa perto do ótimo. A estratégia sequencial clássica da MSR [@boxwilson1951; @boxhunter1957] tem
duas fases: (1) longe do ótimo, um modelo de **primeira ordem** (fatorial $2^k$, sem termos
quadráticos) é suficiente para apontar uma *direção* de melhoria — o **caminho de máxima
inclinação** (*steepest ascent*); (2) perto do ótimo, a curvatura passa a importar, e um desenho de
segunda ordem (como o CCD da próxima seção) é necessário.

```{=html}
<div class="caixa-aplicacao">
<strong>Aplicação — Engenharia de alimentos: secagem de fatias de fruta</strong><br>
Uma planta de desidratação de frutas quer encontrar a combinação de <strong>temperatura</strong>
($30$–$70°\text{C}$) e <strong>tempo de secagem</strong> ($2$–$8$ h) que maximiza a retenção de
vitamina C. Um fatorial $2^2$ inicial, rodado numa região exploratória de baixa temperatura/tempo
curto (onde a equipe suspeitava, sem certeza, que a retenção seria baixa), estima um modelo de
primeira ordem.
</div>
```


``` r
set.seed(2026)
codif_para_natural <- function(x1, x2) tibble(temperatura = 45 + 10 * x1, tempo = 4 + 1.5 * x2)

fatorial_inicial <- expand_grid(x1 = c(-1, 1), x2 = c(-1, 1), rep = 1:3) %>%
  bind_cols(codif_para_natural(.$x1, .$x2)) %>%
  mutate(retencao = 55 + 4 * x1 + 6 * x2 - 1.5 * x1 * x2 + rnorm(n(), 0, 1.5))

modelo_1a_ordem <- lm(retencao ~ x1 + x2, data = fatorial_inicial)
coef(modelo_1a_ordem)
```

```
## (Intercept)          x1          x2 
##   54.132313    4.054148    5.647708
```

O gradiente estimado $(\hat\beta_{x_1}, \hat\beta_{x_2})$ aponta a direção de subida mais rápida em
**unidades codificadas**; convertendo para as unidades naturais originais (multiplicando cada
componente pela meia-amplitude codificada de cada fator, $10°\text{C}$ e $1{,}5\text{h}$) dá o
**passo** a cada movimento ao longo do caminho:


``` r
grad_codificado <- coef(modelo_1a_ordem)[c("x1", "x2")]
passo_unitario <- grad_codificado / sqrt(sum(grad_codificado^2))  # direção unitária

caminho_subida <- tibble(passo = 0:6) %>%
  mutate(
    x1 = passo * passo_unitario["x1"],
    x2 = passo * passo_unitario["x2"],
    temperatura = 45 + 10 * x1,
    tempo       = 4 + 1.5 * x2,
    # retencao "verdadeira" simulada ao longo do caminho, com um maximo por volta de x1=2.5,x2=3.2
    # -- ruido pequeno (sd=0.3) para que o pico fique visível apesar de uma única corrida por passo
    retencao_verdadeira = 75 - 3 * (x1 - 2.5)^2 - 2.5 * (x2 - 3.2)^2 + rnorm(n(), 0, 0.3)
  )
caminho_subida %>% select(passo, temperatura, tempo, retencao_verdadeira) %>%
  kable(digits = 1, caption = "Caminho de máxima inclinação: um novo experimento a cada passo") %>%
  kable_styling(full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:steepest-ascent-passos)Caminho de máxima inclinação: um novo experimento a cada passo</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> passo </th>
   <th style="text-align:right;"> temperatura </th>
   <th style="text-align:right;"> tempo </th>
   <th style="text-align:right;"> retencao_verdadeira </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 30.6 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 50.8 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 49.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 56.7 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 62.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62.5 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 72.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 68.3 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 75.1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 74.2 </td>
   <td style="text-align:right;"> 10.1 </td>
   <td style="text-align:right;"> 72.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 80.0 </td>
   <td style="text-align:right;"> 11.3 </td>
   <td style="text-align:right;"> 64.8 </td>
  </tr>
</tbody>
</table>

<img src="07-superficie-resposta_files/figure-html/steepest-ascent-plot-1.png" alt="" width="80%" style="display: block; margin: auto;" />

Cada passo ao longo do caminho é uma **corrida experimental real** (não uma predição): a equipe
segue na direção do gradiente até que a resposta pare de melhorar. O painel da direita deixa isso
inequívoco — a retenção sobe até o passo 4 e cai nos passos 5–6 — enquanto o painel da esquerda
mostra que essa mesma informação é quase imperceptível só pela cor ao longo do caminho espacial,
porque a escala de cor é dominada pela subida grande dos primeiros passos. É esse declínio, visível
no painel da direita, que sinaliza que a região passou por perto do ótimo e que a
curvatura, ignorada pelo modelo de primeira ordem, começou a importar. É exatamente esse o
sinal para trocar de estratégia: parar de subir e desenhar um experimento de segunda ordem —
tipicamente um CCD — centrado na vizinhança onde o caminho parou de melhorar.

## Delineamento composto central (CCD) {#ccd}

Um fatorial $2^k$ sozinho não estima termos quadráticos $\beta_{ii}$ (só tem dois níveis por
fator). O **delineamento composto central** [@boxwilson1951; @boxhunter1957] aumenta um fatorial
$2^k$ com dois tipos de corrida extra, mantendo a estrutura fatorial como núcleo:

- $2^k$ pontos **fatoriais** (nível $\pm1$ em todos os fatores) — estimam efeitos principais e
  interações, como antes;
- $2k$ pontos **axiais** (**"em estrela"**): cada um varia **um único** fator para $\pm\alpha$,
  com os demais fixos em $0$ — são esses pontos que tornam os termos quadráticos $\beta_{ii}$
  estimáveis;
- $n_c$ pontos **centrais** ($\mathbf{x}=\mathbf{0}$, repetidos $n_c$ vezes) — estimam o erro puro
  e checam curvatura pura antes mesmo de ajustar o modelo completo.

```{=html}
<div class="caixa-r"><strong>Uso do R</strong> — construindo um CCD rotacionável para a secagem de fruta</div>
```


``` r
k_ccd <- 2
alpha_rot <- (2^k_ccd)^(1/4)   # alpha que torna o desenho rotacionavel (Var(y-chapeu) so depende do raio)
nc <- 5                         # pontos centrais (regra pratica: 4-6 para k=2)

ccd_codificado <- bind_rows(
  expand_grid(x1 = c(-1, 1), x2 = c(-1, 1)) %>% mutate(tipo = "fatorial"),
  tibble(x1 = c(-alpha_rot, alpha_rot, 0, 0), x2 = c(0, 0, -alpha_rot, alpha_rot), tipo = "axial"),
  tibble(x1 = rep(0, nc), x2 = rep(0, nc), tipo = "central")
)
nrow(ccd_codificado)   # N = 4 + 4 + 5 = 13 corridas
```

```
## [1] 13
```

A **rotabilidade** — a propriedade de que $\text{Var}(\hat y(\mathbf{x}))$ depende só da distância
$\lVert\mathbf{x}\rVert$ ao centro, não da direção — é obtida escolhendo
$\alpha = (2^k)^{1/4}$ (para $k=2$, $\alpha=$ 1.414); um desenho rotacionável
garante que a precisão da predição não privilegia nenhuma direção do espaço de fatores, desejável
quando não se sabe de antemão em que direção o ótimo vai estar.


``` r
set.seed(2026)
ccd_dados <- ccd_codificado %>%
  mutate(
    temperatura = 65 + 10 * x1,   # CCD centrado onde o caminho de subida parou de melhorar
    tempo       = 6 + 1.5 * x2,
    retencao = 78 - 3 * x1^2 - 2.5 * x2^2 + 0.8 * x1 - 0.5 * x2 - 1.2 * x1 * x2 + rnorm(n(), 0, 1),
    atividade_agua = 0.42 + 0.03 * x1 + 0.025 * x2 + 0.015 * x1^2 + 0.01 * x2^2 -
      0.008 * x1 * x2 + rnorm(n(), 0, 0.01)
  )

ggplot(ccd_dados, aes(x1, x2, color = tipo)) +
  geom_point(size = 3) +
  coord_equal() +
  labs(title = "Estrutura do CCD: núcleo fatorial + pontos axiais + centrais",
       x = expression(x[1]~"(temperatura codificada)"), y = expression(x[2]~"(tempo codificado)")) +
  theme_minimal(base_size = 12)
```

<img src="07-superficie-resposta_files/figure-html/ccd-simulacao-1.png" alt="" width="80%" style="display: block; margin: auto;" />

O gráfico mostra a assinatura geométrica de um CCD: um quadrado (núcleo fatorial), quatro pontos
sobre os eixos além do quadrado (axiais, a distância $\alpha>1$ do centro) e uma pilha de pontos na
origem (centrais) — nenhum desses três grupos, sozinho, estimaria a superfície completa de segunda
ordem; juntos, com $N=13$ corridas, estimam os 6 parâmetros do modelo
($\beta_0,\beta_1,\beta_2,\beta_{11},\beta_{22},\beta_{12}$) com graus de liberdade sobrando para
estimar o erro puro a partir só dos pontos centrais.

## Otimização de múltiplas respostas: funções de desejabilidade {#desejabilidade}

A planta de secagem tem duas respostas em jogo — **retenção de vitamina C** (maximizar) e
**atividade de água residual** (minimizar, por segurança microbiológica) — e o ponto que maximiza
uma pode não ser o que minimiza a outra. A abordagem de **desejabilidade**
[@derringersuich1980] converte cada resposta $\hat y_j(\mathbf{x})$ numa escala comum
$d_j(\mathbf{x})\in[0,1]$ (0 = inaceitável, 1 = ideal) e combina as $m$ desejabilidades individuais
pela **média geométrica**,
$$
D(\mathbf{x}) = \Big(\prod_{j=1}^m d_j(\mathbf{x})\Big)^{1/m},
$$
de modo que $D=0$ se **qualquer** resposta for inaceitável (a média geométrica penaliza um único
$d_j=0$ derrubando o produto inteiro — uma média aritmética não teria essa propriedade). Para uma
resposta a **maximizar** entre um mínimo aceitável $L$ e um alvo $T$,
$d_j = \big[(\hat y_j-L)/(T-L)\big]^{s}$ (truncado em $[0,1]$); para **minimizar** entre um alvo $T$
e um máximo aceitável $U$, $d_j = \big[(U-\hat y_j)/(U-T)\big]^{s}$; o expoente $s$ controla quão
rígida é a aproximação à meta ($s=1$: linear).


``` r
modelo_retencao <- lm(retencao ~ x1 + x2 + I(x1^2) + I(x2^2) + x1:x2, data = ccd_dados)
modelo_atividade <- lm(atividade_agua ~ x1 + x2 + I(x1^2) + I(x2^2) + x1:x2, data = ccd_dados)
```


``` r
desej_maximizar <- function(y, L, T_alvo) pmin(pmax((y - L) / (T_alvo - L), 0), 1)
desej_minimizar <- function(y, T_alvo, U) pmin(pmax((U - y) / (U - T_alvo), 0), 1)

grade_ccd <- expand_grid(x1 = seq(-alpha_rot, alpha_rot, length.out = 60),
                          x2 = seq(-alpha_rot, alpha_rot, length.out = 60)) %>%
  mutate(
    retencao_pred  = predict(modelo_retencao, newdata = .),
    atividade_pred = predict(modelo_atividade, newdata = .),
    d_retencao  = desej_maximizar(retencao_pred, L = 65, T_alvo = 78),
    d_atividade = desej_minimizar(atividade_pred, T_alvo = 0.35, U = 0.55),
    D = sqrt(d_retencao * d_atividade)   # media geometrica, m=2
  )

melhor_ponto <- grade_ccd %>% slice_max(D, n = 1)

ggplot(grade_ccd, aes(x1, x2, fill = D)) +
  geom_raster() +
  geom_contour(aes(z = D), color = "white", alpha = 0.4) +
  geom_point(data = melhor_ponto, aes(x1, x2), color = "red", size = 3) +
  scale_fill_viridis_c(name = "Desejabilidade\nglobal D") +
  coord_equal() +
  labs(title = "Desejabilidade global: retenção de vitamina C (max)\ne atividade de água (min)",
       x = expression(x[1]), y = expression(x[2])) +
  theme_minimal(base_size = 12)
```

<img src="07-superficie-resposta_files/figure-html/ccd-desejabilidade-1.png" alt="" width="80%" style="display: block; margin: auto;" />

O ponto vermelho — o máximo de $D(\mathbf{x})$ na grade — não coincide nem com o máximo isolado da
retenção nem com o mínimo isolado da atividade de água; é o **compromisso** que a média geométrica
das duas desejabilidades encontra, exatamente o problema que motivou a técnica. Na escala natural,
esse ponto corresponde a temperatura $\approx$ 62.4°C e
tempo $\approx$ 5.5 h, com desejabilidade global
$D\approx$ 0.81.

## Desenho robusto: otimizando a média e a variância ao mesmo tempo {#desenho-robusto}

Toda a metodologia deste capítulo, até aqui, otimiza uma **média** — o ponto estacionário, a
análise de ridge, a desejabilidade combinam previsões de $E[y\mid\mathbf{x}]$. Genichi Taguchi
[-@taguchi1986] levantou, na engenharia da qualidade japonesa dos anos 1980, uma pergunta
diferente: além de acertar um alvo, um processo de manufatura precisa ser **robusto** — pouco
sensível a fatores de ruído (variação de matéria-prima, desgaste de equipamento, condições
ambientais) que o engenheiro não controla em produção, mesmo que os controle perfeitamente durante
o experimento. Dois processos podem ter a mesma média e diferir enormemente em quão essa média se
mantém estável quando o ruído de produção entra em cena — e MSR clássica, focada só na média, não
enxerga essa diferença.

```{=html}
<div class="caixa-aplicacao">
<strong>Aplicação — Engenharia: espessura de um revestimento por deposição</strong><br>
Um processo de deposição controla dois fatores — <strong>temperatura</strong> e
<strong>pressão</strong> da câmara — para produzir um revestimento de espessura alvo
15&nbsp;&micro;m. Além de acertar a média, a fábrica quer <strong>variância mínima</strong> em
torno do alvo: um lote com espessura média 15 mas desvio-padrão alto tem mais peças fora da
tolerância do que um lote com a mesma média e desvio-padrão baixo.
</div>
```

### A proposta original de Taguchi, e por que ela foi revista

Taguchi propôs cruzar um **arranjo interno** de fatores de controle com um **arranjo externo** de
fatores de ruído (simulados deliberadamente no experimento, por exemplo variando a matéria-prima
entre lotes conhecidos de qualidade diferente), calculando para cada combinação de controle uma
**razão sinal-ruído** agregada sobre todas as réplicas de ruído, e escolhendo os níveis de controle
que maximizam essa razão. A ideia — otimizar robustez, não só média — foi influente e
genuinamente nova; o **método** teve, porém, duas críticas estatísticas que se consolidaram nas
décadas seguintes [@viningmyers1990]: (i) arranjos internos $\times$ externos cruzados exigem
muito mais corridas do que uma única superfície de resposta bem desenhada; e (ii) a razão
sinal-ruído comprime média e variância numa única estatística, perdendo informação sobre como cada
uma responde separadamente aos fatores de controle — informação que a própria maquinaria deste
capítulo (modelo de segunda ordem, ponto estacionário, análise canônica) já sabe extrair.

### A alternativa: duas superfícies de resposta, uma única maquinaria

Vining e Myers [-@viningmyers1990] propuseram tratar o problema de Taguchi com as ferramentas já
construídas neste capítulo: em vez de um arranjo externo separado, usa-se um único delineamento com
**réplicas genuínas** em cada ponto (o CCD da Seção \@ref(ccd) serve para isso sem alteração
nenhuma) e ajustam-se **duas** superfícies de segunda ordem a partir dos mesmos dados — uma para a
média $\hat y(\mathbf{x})$ em cada ponto, outra para o logaritmo da variância
$\ln\widehat{\sigma^2}(\mathbf{x})$ (o log estabiliza a variância da própria estimativa de
variância, pela mesma razão da transformação de Box-Cox da Seção \@ref(transformacoes-dca)). O
problema de otimização vira: minimizar $\widehat{\sigma^2}(\mathbf{x})$ sujeito a
$\hat y(\mathbf{x})$ ficar dentro de uma faixa aceitável ao redor do alvo — exatamente o tipo de
otimização restrita que a Seção \@ref(desejabilidade) já resolveu por busca em grade, agora com uma
restrição em vez de uma segunda desejabilidade.

```{=html}
<div class="caixa-r"><strong>Uso do R</strong> — duas superfícies de resposta (média e log-variância) a partir de um único CCD replicado</div>
```


``` r
set.seed(2026)
pontos_robusto <- bind_rows(
  expand_grid(x1 = c(-1, 1), x2 = c(-1, 1)),
  tibble(x1 = c(-alpha_rot, alpha_rot, 0, 0), x2 = c(0, 0, -alpha_rot, alpha_rot)),
  tibble(x1 = 0, x2 = 0)
)
r_robusto <- 8   # replicas genuinas por ponto do desenho -- e o que permite estimar variancia local

dados_robusto <- pontos_robusto %>%
  slice(rep(1:n(), each = r_robusto)) %>%
  mutate(
    temperatura = 180 + 10 * x1,
    pressao     = 40 + 5 * x2,
    mu_true    = 15 + 0.3*x1 - 0.2*x2 - 0.15*x1^2 - 0.1*x2^2 + 0.05*x1*x2,
    sigma_true = exp(0.35 + 0.30*x1 + 0.25*x2^2),   # variancia sobe com temperatura, e com |pressao| fora do centro
    espessura  = rnorm(n(), mu_true, sigma_true)
  )

resumo_robusto <- dados_robusto %>%
  group_by(x1, x2) %>%
  summarise(media = mean(espessura), variancia = var(espessura), .groups = "drop")

resumo_robusto %>%
  kable(digits = 2, caption = "Média e variância amostrais (8 réplicas) em cada um dos 9 pontos do CCD") %>%
  kable_styling(full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:robusto-dados)Média e variância amostrais (8 réplicas) em cada um dos 9 pontos do CCD</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> x1 </th>
   <th style="text-align:right;"> x2 </th>
   <th style="text-align:right;"> media </th>
   <th style="text-align:right;"> variancia </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> -1.41 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 14.38 </td>
   <td style="text-align:right;"> 0.45 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> -1.00 </td>
   <td style="text-align:right;"> -1.00 </td>
   <td style="text-align:right;"> 13.78 </td>
   <td style="text-align:right;"> 1.59 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> -1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 13.67 </td>
   <td style="text-align:right;"> 2.11 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -1.41 </td>
   <td style="text-align:right;"> 14.64 </td>
   <td style="text-align:right;"> 2.73 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 14.47 </td>
   <td style="text-align:right;"> 2.25 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 13.49 </td>
   <td style="text-align:right;"> 1.63 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> -1.00 </td>
   <td style="text-align:right;"> 15.29 </td>
   <td style="text-align:right;"> 4.98 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 16.67 </td>
   <td style="text-align:right;"> 5.36 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 15.02 </td>
   <td style="text-align:right;"> 4.60 </td>
  </tr>
</tbody>
</table>


``` r
modelo_media_r  <- lm(media ~ x1 + x2 + I(x1^2) + I(x2^2) + x1:x2, data = resumo_robusto)
modelo_logvar_r <- lm(log(variancia) ~ x1 + x2 + I(x1^2) + I(x2^2) + x1:x2, data = resumo_robusto)
```


``` r
grade_robusto <- expand_grid(x1 = seq(-alpha_rot, alpha_rot, length.out = 80),
                              x2 = seq(-alpha_rot, alpha_rot, length.out = 80)) %>%
  mutate(
    media_pred = predict(modelo_media_r, newdata = .),
    var_pred   = exp(predict(modelo_logvar_r, newdata = .)),
    viavel     = abs(media_pred - 15) < 0.5   # faixa aceitavel ao redor do alvo de 15 um
  )

ponto_robusto <- grade_robusto %>% filter(viavel) %>% slice_min(var_pred, n = 1)

ggplot(grade_robusto, aes(x1, x2)) +
  geom_raster(aes(fill = var_pred)) +
  geom_contour(aes(z = media_pred), breaks = c(14.5, 15, 15.5), color = "white", linewidth = 0.6) +
  geom_point(data = ponto_robusto, aes(x1, x2), color = "red", size = 3) +
  scale_fill_viridis_c(name = expression(hat(sigma)^2), option = "magma") +
  coord_equal() +
  labs(title = "Variância prevista (cor) e média prevista\n(contornos brancos, 14,5/15/15,5 µm)",
       x = expression(x[1]~"(temperatura codificada)"), y = expression(x[2]~"(pressão codificada)")) +
  theme_minimal(base_size = 12)
```

<img src="07-superficie-resposta_files/figure-html/robusto-plot-1.png" alt="" width="80%" style="display: block; margin: auto;" />

O contorno branco central marca onde a média prevista cruza exatamente o alvo de 15 µm; a cor de
fundo mostra que a variância prevista **cresce com a temperatura** ($x_1$ alto) e **cresce nas duas
direções conforme a pressão se afasta do centro** ($x_2^2$) — duas conclusões que a razão
sinal-ruído de Taguchi, agregada num único número, jamais teria separado uma da outra. O ponto
vermelho — o de menor variância prevista dentre os que ficam a $\pm0{,}5$ µm do alvo — fica em
temperatura $\approx$ 165.9°C (baixa) e pressão $\approx$
34.5 (próxima do centro), com variância prevista
$\hat\sigma^2\approx$ 0.82 — bem abaixo da variância observada em
qualquer ponto de temperatura alta da Tabela acima. A recomendação final para a fábrica: **não** é
o ponto de temperatura mais alta (ainda que ele também passe perto do alvo), porque ali a variância
prevista é ordens de grandeza maior — o mesmo tipo de compromisso média-variância que motivou
Taguchi, resolvido aqui com duas superfícies de resposta interpretáveis separadamente, em vez de
uma razão sinal-ruído que as mistura.

## Resumo do capítulo

- A metodologia de superfície de resposta troca a pergunta "o que importa?" (típica dos
  fatoriais) pela pergunta "qual é o ótimo?", ajustando um modelo de segunda ordem que captura
  curvatura — algo que nenhum fatorial de dois níveis consegue estimar.
- O ponto estacionário resolve $\mathbf{b}+2\mathbf{B}\mathbf{x}_0=\mathbf{0}$; os autovalores da
  Hessiana $\mathbf{B}$ classificam-no como máximo, mínimo ou sela — no exemplo da energia de
  corte, uma sela, o que desloca a busca pelo ótimo prático para a fronteira da região
  experimental.
- A análise canônica (autovetores, não só autovalores, de $\mathbf{B}$) reescreve o modelo numa
  base rotacionada onde a curvatura fica diagonal, revelando a direção em que a superfície muda
  mais rápido — informação que a classificação simples do ponto estacionário não dá.
- A análise de ridge otimiza dentro de um raio fixo do centro do desenho (em unidades codificadas),
  produzindo um caminho de pontos ótimos-restritos — uma alternativa disciplinada a extrapolar até
  um ponto estacionário fora da região onde os dados foram coletados.
- O caminho de máxima inclinação usa um modelo de primeira ordem, longe do ótimo, para apontar uma
  direção de melhoria com corridas reais sequenciais; quando a resposta para de melhorar (a
  curvatura passa a importar), é hora de desenhar um experimento de segunda ordem.
- O delineamento composto central (CCD) — núcleo fatorial $2^k$ + pontos axiais + pontos centrais —
  é o desenho de segunda ordem padrão; a escolha $\alpha=(2^k)^{1/4}$ o torna rotacionável.
- Quando há mais de uma resposta em jogo, funções de desejabilidade combinam metas individuais
  (maximizar, minimizar ou mirar um alvo) numa única escala $[0,1]$ via média geométrica, que
  penaliza qualquer resposta inaceitável isoladamente.
- *(Além do programa do semestre)* Desenho robusto pergunta não só "qual ponto acerta o alvo?" mas
  "qual ponto é **insensível** a ruído de produção?": réplicas genuínas num único CCD alimentam
  **duas** superfícies de segunda ordem — uma para a média, outra para a log-variância —, que
  Vining e Myers mostraram serem estatisticamente mais eficientes e mais interpretáveis do que a
  razão sinal-ruído original de Taguchi.

## Fim do programa do semestre

Este capítulo fecha os sete capítulos alinhados ao cronograma de MATD48 — a jornada que começou no
Capítulo 1 com uma distinção aparentemente simples, unidade experimental versus unidade amostral, e
terminou aqui ajustando superfícies de segunda ordem sobre fatoriais completos de múltiplos
fatores. A própria MSR tem, hoje, uma contraparte para respostas obtidas por simulação
computacional em vez de experimento físico — os **desenhos de experimentos computacionais**, que
substituem réplicas e erro aleatório por desenhos "preenchedores de espaço" (*space-filling*,
como hipercubos latinos) e modelos de processo gaussiano no lugar do polinômio de segunda ordem
[@sacks1989; @santner2003design], uma direção que foge do escopo deste curso mas que compartilha
o mesmo objetivo de mapear uma superfície de resposta com o menor número de corridas possível. O
Capítulo 8 estende o livro (não o semestre) para o domínio de experimentação onde o delineamento
clássico encontrou, nas últimas duas décadas, sua aplicação mais numerosa: testes A/B e bandits em
produtos digitais — o mesmo arcabouço causal do Capítulo 1, a mesma lógica de aleatorização e
controle de confusão, aplicados a um contexto em que os experimentos são sequenciais, numerosos e
frequentemente automatizados.
