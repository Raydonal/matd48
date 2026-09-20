# Plano de conteúdo — Livro, Aulas2026, Listas2026

Documento de rastreabilidade entre sessões para o projeto de reescrita do curso MATD48
(planejamento e análise de experimentos) em três entregáveis: livro bookdown (`Livro/`),
slides refeitos (`Aulas2026/`) e listas semanais em LaTeX com gabarito (`Listas2026/`).

Fonte de referência de estrutura/tópicos (não de texto — ver nota de originalidade):
`ApoioLuz/Diseño y Análisis de Experimentos - Luz Mery González García - Corregido.pdf`
(UNAL, 2024, 6 capítulos, 484 p., espanhol) + `ApoioLuz/programa diseño II-2025.pdf` (cronograma
de 16 semanas da autora).

**Nota de originalidade:** nenhum texto do PDF é traduzido/copiado. O livro-fonte serve só como
guia de progressão de tópicos (que é conteúdo estatístico padrão, não autoral). Todo texto,
exemplos, dados simulados e exercícios em `Livro/`, `Aulas2026/` e `Listas2026/` são redigidos do
zero em português. As bases de dados reais do livro-fonte (`ApoioLuz/BasesDatosDE.xlsx`) foram
extraídas para `Livro/data/*.csv` porque dados brutos não são objeto de direito autoral.

## Bases de dados disponíveis

Extraídas de `ApoioLuz/BasesDatosDE.xlsx` para `Livro/data/`:

| arquivo | colunas | contexto | uso sugerido |
|---|---|---|---|
| `mojarra.csv` (100 linhas) | probiotico, Acuario, Peso Inicial, Peso 45 | aquicultura — probiótico no ganho de peso de tilápia, com submuestreo por aquário | DCA com submuestreo (aula 05) |
| `pepino.csv` (24 linhas) | riego, silicio, bloque, altura | agricultura — irrigação × dose de silício em blocos, altura de pepino | fatorial em blocos (aula 09/12) |
| `acuosas.csv` (54 linhas) | Viscosidad, Salinidad, Tiempo, inyeccion, recupera | processo industrial — recuperação de compostos voláteis, fatorial A×B×C | fatorial de 3 fatores (aula 12) |
| `biodiesel.csv` (32 linhas) | Relacion, Catalizador, Temperatura, Agente, Rendimiento (níveis codificados -1/1) | engenharia química — rendimento de biodiesel, fatorial 2^4 | fatorial 2^k (aula 13) |
| `energia.csv` (36 linhas) | Velocidad, angulo, energia | usinagem/engenharia — energia de corte vs velocidade e ângulo | superfície de resposta (aula 14) |

Datasets de **psicologia** e parte de **ciência de dados** não existem prontos — cada módulo os
simula inline no `.Rmd` com `set.seed()` (convenção já usada em `index.Rmd` do repositório),
documentando o cenário no próprio texto. Sugestões já definidas para manter coerência entre
capítulo do livro, aula e lista (mesmo dataset/cenário nos três, para reforço pedagógico):

| aula | cenário de psicologia | cenário de ciência de dados/engenharia | cenário de agricultura |
|---|---|---|---|
| 01 | técnicas de estudo (flashcards/releitura/teste-prática) e nota em prova | — | plantio (variedades de semente) como contraponto |
| 02–03 | tempo de reação vs. horas de sono (regressão) | tempo de resposta de API vs. nº de requisições (regressão, matriz de projeção) | `Aulas/ad_spend_and_gdp.csv` como exemplo numérico extra de OLS, se útil |
| 04–06 | tempo de reação sob distração (nenhuma/sonora/visual), com submuestreo (múltiplas tentativas por sujeito) | — | `mojarra.csv` (submuestreo real) |
| 07–08 | dose de cafeína (0/50/100/150/200 mg) vs. desempenho em teste de atenção (polinômios ortogonais) | tempo de carregamento de página vs. nº de scripts (contrastes/comparações múltiplas) | — |
| 09–11 | bloco = sujeito (medidas repetidas) em desenho terapêutico | — | `pepino.csv` (blocos reais) |
| 12 | — | teste A/B/n em site (cor do botão × layout × texto de CTA) na taxa de conversão | `acuosas.csv` / `pepino.csv` |
| 13–14 | — | `biodiesel.csv` (2^4, confusão/fracionamento), `energia.csv` (superfície de resposta) | — |

## Estrutura das aulas (Aulas2026/)

Cada `Aulas2026/MATD48-NN.Rmd` (xaringan) segue 4 blocos de seções, nesta ordem:
1. **Teoria** — definição formal, resultado/fórmula, breve justificativa.
2. **Aplicação** — exemplo contextualizado completo (um dos cenários acima), com dados e R.
3. **Discussão** — 2–4 perguntas abertas para a turma (interpretação, limites, decisões de desenho).
4. **Uso do R** — trecho de código comentado, reprodutível, plugado no exemplo da seção Aplicação.

## Estrutura do livro (Livro/)

Bookdown, gitbook, renderizado in-place em `Livro/` (mesmo padrão do repo: fonte e HTML juntos).
6 capítulos, sem seção de exercícios (exercícios só em `Listas2026/`, para não duplicar).
Ver tabela de mapeamento capítulo → aulas no plano aprovado (`~/.claude/plans/ticklish-cuddling-graham.md`).

## Estrutura das listas (Listas2026/)

`ListaNN.tex` (4–6 problemas contextualizados) + `GabaritoNN.tex` (solução completa, mesma
numeração), ambos usando `Listas2026/preamble.tex` compartilhado. Uma lista por aula (14 ao todo).

## Padrão de rigor exigido

Correção explícita do usuário (professor da disciplina), após ver o material inicial: é um curso
avançado, para alunos fortes — o material não pode ser raso. A partir deste ponto, todo capítulo/
aula/lista deve:

1. Apresentar **notação matricial** sempre que possível ($Y=X\beta+\varepsilon$, matriz de
   projeção $H=X(X'X)^-X'$, somas de quadrados como formas quadráticas $y'Ay$), não só a notação
   escalar por índices — mesmo fora do Capítulo 2, conectando de volta a ele.
2. Expor a **estrutura completa do dado** em toda aplicação — por que o experimento gera aquele
   formato de tabela, antes de rodar `aov()`/`lm()` diretamente.
3. Formalizar a **camada de inferência causal** (resultados potenciais $Y_i(t)$, SUTVA, ATE,
   argumento de Neyman/Rubin para não-viés sob aleatorização) nos capítulos que comparam
   tratamentos, na linha das notas de planejamento de experimentos de Art B. Owen (Stanford) e do
   modelo causal de Neyman-Rubin. Referências reais: Rubin (1974), Neyman (1923/1990),
   Imbens & Rubin (2015) — já adicionadas a `Livro/refs.bib`.
4. Incluir pelo menos uma questão de **dedução/prova** por lista de exercícios, com solução
   completa no gabarito.
5. **Parte gráfica robusta em todo lugar**: todo ajuste de modelo vem acompanhado de gráfico(s)
   com discussão explícita do resultado; reaproveitar `Aulas/images/` (material de slides do
   professor) para complementar o livro onde fizer sentido temático.

Ver memória de sessão `matd48-rigor-padrao` para o texto completo desta diretriz.

## Status

- [x] Infra: pastas criadas, pacotes R instalados (bookdown, xaringan, readxl), dados extraídos.
- [x] Piloto: capítulo 1 + aula 01 + lista 01 + gabarito 01 — renderizado e validado (bookdown,
      xaringan e latexmk todos sem erro). Define o padrão de estilo para os módulos seguintes.
- [x] Módulo Modelos Lineares (cap.2, aulas 02–03, listas 02–03) — completo e verificado.
- [x] Módulo DCA completo (cap.3 inteiro, aulas 04–08, listas 04–08) — retrabalhado por um único
      agente (corrigindo a condição de corrida anterior) após correção do professor. Adicionado:
      teste de Duncan (ausente antes; livro + aula 07, com `agricolae::duncan.test()`), seção
      "O perigo do data snooping" antes do método de Scheffé (livro + aula 07), painel 4x4
      linear/quadrático/cúbico/quártico com R² no exemplo de cafeína (estilo "fio de algodão" das
      aulas antigas, livro + aula 07), boxplot de resíduos por grupo ao lado do Q-Q plot na seção
      de pressupostos (estilo "experimento dos porcos", livro + aula 06). Corrigido erro numérico
      real: a redução de variância da média de UE ao passar de q=5 para q=20 submuestras estava
      reportada como "~10%" quando o valor correto (verificado numericamente) é ~27%; texto agora
      usa `r round()`/`scales::percent()` em vez de número fixo. Notação $\tau_i$ (efeito de
      tratamento) confirmada consistente em todo o capítulo, sem sobreposição com blocos (fora de
      escopo do Cap. 3). Verificado contra ApoioLuz cap. 3 (comparações múltiplas): cobertura já
      igualava ou excedia a fonte (Scheffé/Bonferroni/Tukey-Kramer/Dunnett + Duncan extra).
      Referência Duncan (1955) adicionada a `Livro/refs.bib` e `Aulas/refs.bib`. Verificado:
      bookdown (exit 0), 5 decks xaringan 04–08 (exit 0), 10 PDFs de lista/gabarito via latexmk
      (exit 0).
- [x] Módulo Blocos (cap.4, aulas 09–11, listas 09–11) — completo e verificado.
- [x] Módulo Fatoriais (cap.5–6, aulas 12–14, listas 12–14) — completo e verificado (bookdown,
      3 decks, 6 PDFs de lista/gabarito), com notação matricial, causal e gráficos reforçados.
- [x] Consolidação (1ª rodada): render-all limpo, `index.Rmd`/`index.html` reescritos para 2026
      (mesmos dias/horários — Terça PAF1 sala 208 e Quinta Lab 140 IME, 10:40–12:30), site anterior
      arquivado intacto em `index-2025.Rmd`/`index-2025.html`, `CLAUDE.md` atualizado.
- [x] **Retrabalho profundo (2ª rodada)**, após correção explícita do professor ("erros de
      notação", "não aproveitou meus slides do ano passado nem o livro da Luz Mery", "faltam
      gráficos em quase todas partes", "slides deste ano ficaram fracos"). Cada módulo minerou
      `Aulas/MATD48-NN.Rmd` (ano anterior) e o PDF da Luz Mery em profundidade. Achados/correções
      concretos:
      - Cap.2: teorema de Frisch-Waugh-Lovell (prova completa, ausente antes) + pseudoinversa de
        Moore-Penrose via SVD (comparada a `MASS::ginv()`), ambos minerados de `Aulas/MATD48-03/05`.
      - Cap.1: simulação Monte Carlo (5000 reatribuições) mostrando $E[\widehat{ATE}]=ATE$.
      - Nota de convenção de notação ($\tau_i \to \alpha_i,\beta_j,\gamma_k$) adicionada no início
        do Cap.5, evitando a troca de símbolo sem aviso identificada no diagnóstico.
      - Cap.3: teste de Duncan (ausente), seção "perigo do data snooping", painel 4×4 de polinômios
        (estilo "fio de algodão"), diagnóstico de resíduos estilo "experimento dos porcos". **Bug
        numérico real corrigido**: redução de variância reportada como "~10%" (era ~27%, valor
        agora calculado dinamicamente em vez de hard-coded).
      - Cap.4: mapa real dos bairros do Recife (`Aulas/Bairros_Recife/`) como exemplo de blocagem
        geográfica, seções de E(QM) formalizando a invalidade do teste F de bloco, algoritmo de
        aleatorização de Yates (1933) em R, análise de potência para escolha de réplicas de
        quadrado latino.
      - Cap.5–6: progressão visual do cubo $2^3$ (cubo → efeito principal → interação dupla →
        tripla) com dados reais de `biodiesel.csv`. **Dois bugs numéricos reais corrigidos**: (1)
        `factor()` nos níveis codificados ±1 quebrava silenciosamente a ortogonalidade e invalidava
        toda a análise de Lenth/half-normal/Pareto (nenhum efeito cruzava a margem, contradizendo
        o texto) — corrigido mantendo os fatores numéricos; (2) um "mínimo" da superfície de
        resposta era na verdade um **ponto de sela** (autovalores do Hessiano de sinais opostos) —
        corrigido com derivação matricial do gradiente/Hessiano.
      - Todas as 28 listas/gabaritos ganharam questões extras onde fazia sentido (SVD/FWL em
        Lista02/03, fatores confundidos em Lista09, Yates/potência em Lista11).
- [x] Consolidação final (após retrabalho): render-all limpo — livro (6 capítulos, exit 0), 14
      decks de `Aulas2026/` (exit 0 cada), 28 PDFs de `Listas2026/` via latexmk (exit 0) — e
      spot-check confirmando que as correções específicas (FWL, Duncan, Recife, convenção de
      notação, fix do `factor()`) estão presentes e estáveis em disco.
- [x] 3 projetos-desafio criados (`Projetos/Projeto-II/III/IV.Rmd`), linkados no cronograma da
      home: Projeto II (helicóptero de papel, DCA físico), Projeto III (comparação de modelos de
      ML como DBCA + teste de Friedman), Projeto IV (fatorial $2^4$ + superfície de resposta em
      pipeline de ML, capstone). Todos renderizam HTML/PDF sem erro.
- [x] **3ª rodada**, após nova correção do professor ("ainda fraco... queria deixar mais completo
      como o livro da Luz e o doenotes.pdf do Art Owen... precisa discutir método científico e
      causalidade... projetos incrementais divididos em 3 períodos, avaliação = 3 notas (projeto +
      listas)... faltam imagens/gráficos... corrige os deste ano"). Minerado `Material/doenotes.pdf`
      (Owen, Stanford Stat 263/363) e o prefácio/estrutura de `ApoioLuz/...Luz Mery...pdf` para guiar
      (não copiar) os acréscimos:
      - Cap.1: nova seção de abertura "O método científico e o papel do delineamento experimental"
        (ciclo empírico, falseabilidade de Popper, observação vs. experimentação, nota histórica
        Fisher/Rothamsted, confusão/*confounding* com exemplo concreto ao estilo Owen) antes da
        seção de unidades experimentais; nova subseção "Validade interna e validade externa" logo
        após a prova de $E[\widehat{ATE}]=ATE$. Referências novas em `Livro/refs.bib` e
        `Aulas/refs.bib`: Popper (1959), Owen (2020, doenotes). Resumo do capítulo atualizado.
        Livro inteiro re-renderizado (`bookdown::render_book`, exit 0).
      - `Listas2026/Lista01.tex`/`Gabarito01.tex`: questão 7 nova (falseabilidade + validade
        externa, cenário de teste beta em e-commerce), com solução completa. Compilado via
        latexmk (exit 0, Lista01.pdf 3 p., Gabarito01.pdf 4 p.).
      - `index.Rmd`: sistema de avaliação trocado de 2 Avaliações escritas (50/50) para **3 notas
        parciais** (N1/N2/N3), cada uma = 50% projeto do período + 50% média das listas do mesmo
        período, alinhadas aos 3 projetos já existentes (N1↔Projeto II↔aulas 01–08, N2↔Projeto
        III↔aulas 09–11, N3↔Projeto IV↔aulas 12–14); $M = (N1+N2+N3)/3$. Cronograma atualizado
        (linhas "AVALIAÇÃO 1/2" substituídas por linhas "Fechamento N1/N2/N3"). Re-renderizado.
      - `Projetos/Projeto-II/III/IV.Rmd`: adicionada nota de avaliação (qual nota parcial cada
        projeto fecha) e reforço explícito do caráter incremental (cada projeto nomeia o que retoma
        do anterior: II→III retoma a lição de réplica única virando validação cruzada em blocos;
        III→IV retoma o mesmo pipeline/fintech CrediScore, agora num fatorial $2^4$). Todos
        re-renderizados em HTML (exit 0).
      - Disparados em paralelo (background) dois agentes para completar a rodada: (a) reforço de
        gráficos/imagens reaproveitando `Aulas/images/` nos capítulos 2–6 do livro; (b) auditoria e
        aprofundamento dos 14 decks de `Aulas2026/` contra o livro reforçado e os slides antigos.
        Ver entradas de status subsequentes para o resultado consolidado de cada um.
      - **Agente (b) concluído**: 4 dos 14 decks precisaram de reforço concreto (os demais já
        estavam sólidos da rodada anterior). `MATD48-01`: 4 slides de teoria novos cobrindo método
        científico/Popper, nota histórica Fisher-Rothamsted e confusão/validade interna-externa
        (o bloco de Discussão já perguntava sobre isso, mas a Teoria nunca definia os termos —
        lacuna aberta pelo reforço do Cap.1 do livro), com a imagem `Aulas/images/circular_flowchart.png`
        reaproveitada do deck antigo. `MATD48-04` (DCA): formalização causal específica do DCA
        (SUTVA, $\tau_{i,k}$, proposição de Neyman) + notação matricial $\hat\beta=(X'X)^{-1}X'Y$
        que faltavam frente ao Cap.3. `MATD48-05` (submuestreo): 2 slides com a matriz de
        covariância de simetria composta $V=\sigma^2 ZZ'+\sigma^2 I$. `MATD48-09` (DBCA): imagem
        `Aulas/images/quadro5Blocos.png` (tabela bloco×tratamento) ilustrando a estrutura de dados
        já discutida em texto. Nenhum erro técnico real encontrado nos demais decks; valores
        hard-coded (ex. Friedman $\chi^2=2{,}09$, $p=0{,}35$ na Aula 10) conferidos numericamente e
        batem; bug conhecido do `factor()` no $2^4$ (Aula 13) confirmado ainda correto. Todos os 14
        decks renderizam exit 0 (`rmarkdown::render`), individualmente e em lote.
      - **Agente (a) concluído** (reforço gráfico Cap. 2–6): 5 imagens de `Aulas/images/`
        reaproveitadas (confirmadas por grep antes do reuso) — `experimento1.png`+`sit2.png` no
        Cap.3 (submuestreo, layouts UE-com-submuestreo vs. UE=UO), `sit4.png` no Cap.4 (DBCA, logo
        após o modelo), `dadosyates.png`+`yatesout.png` no Cap.6, ancorando uma seção nova "O
        algoritmo tabular de Yates" (derivação do atalho de somas/diferenças de Yates 1937 a partir
        do produto interno já estabelecido, implementado em R e verificado numericamente contra os
        efeitos do $2^4$ do biodiesel via regressão — diferença máxima zero, checado por mim
        independentemente). Mais 5 gráficos ggplot novos com discussão (médias±IC e resíduos
        entre/dentro-aquário no Cap.3, painel de efeitos marginais e comparação de QM-erro
        bloqueado/não-bloqueado no Cap.5). Cap.2 e Cap.5 não ganharam imagens antigas por já terem
        cobertura equivalente ou superior (gráficos data-driven já presentes). Re-render completo
        do livro (`bookdown::render_book`, `rm -rf _bookdown_files` antes) verificado
        independentemente, exit 0.
      - **3ª rodada concluída**: todas as 5 frentes (Cap.1, Lista01/Gabarito01, index.Rmd,
        Projetos II–IV, reforço gráfico do livro, auditoria Aulas2026) entregues e verificadas por
        render/compilação limpa. Nada commitado ainda (aguardando decisão do professor sobre git).
- [x] **4ª rodada**, após correção mais dura do professor ("o livro foi praticamente construído em
      base nos slides e não está aproveitando o material de Luz e de Owen... amplie o escopo...
      não colocou nada de testes A/B... uma coisa é o livro que é geral, outra são as notas de aula
      restritas ao semestre"). Mudança de princípio: `Livro/` deixou de ser um espelho 1:1 do
      cronograma de `Aulas2026/` — agora tem **7 capítulos**, 6 alinhados ao semestre + 1 que o
      excede deliberadamente.
      - **Capítulo 7 novo** (`Livro/07-ab-testing-bandits.Rmd`, 734 linhas): "Experimentação em
        produtos digitais: testes A/B e bandits", minerando `Material/doenotes.pdf` (Owen, cap.
        1-3) para progressão de tópicos, prosa 100% original. Parte I amarra teste A/B ao DCA do
        Cap.3 e ao modelo de resultados potenciais do Cap.1 (aleatorização por hash, efeito
        pequeno/variância grande, problema do *peeking* com SPRT de Wald — simulação MC mostrando
        inflação de falso-positivo de ~5% para ~25% sob checagem repetida —, vício do vencedor).
        Parte II cobre bandits (regret, UCB1, Thompson sampling) com simulação real comparando as
        3 políticas (regret final: aleatória≈90, UCB1≈75, Thompson≈39, consistente com a teoria).
        Seção final aponta (sem desenvolver) experimentos computacionais/space-filling e Taguchi.
        Registrado em `_bookdown.yml`; `Livro/index.Rmd` e a home do curso atualizados para "7
        capítulos, 6 do semestre + 1 além dele"; a seção "Encerrando o livro" foi movida do fim do
        Cap.6 para o fim do Cap.7.
      - **Cap.1-6 reforçados de novo**, agora minerando Luz/Owen capítulo a capítulo (não só
        reforço pontual): Cap.1 ganhou a fórmula de variância finita de Neyman para o ATE-chapéu
        (verificada numericamente contra a MC já existente) + tabela científica (science table) +
        imagem do ciclo empírico; Cap.2 (que tinha zero diagramas conceituais apesar de ser o mais
        algébrico) ganhou 2 diagramas (geometria de projeção, partição de X em blocos); Cap.4
        ganhou a dedução completa da análise intra-bloco ajustada de BIB ($Q_i$, $SQ_{Trat(adj)}$),
        verificada numericamente contra `aov()`; Cap.5 (o mais fraco, 4→9 citações) ganhou a tabela
        completa de E[QM] do fatorial A×B ligada ao qui-quadrado não-central do Cap.2; Cap.6 foi de
        4→9 citações. Referências novas usadas: `yates1937design`, `finney1945fractional`,
        `taguchi1986`, `sacks1989`, `santner2003design`, `kirk2012experimental`, `dean2017design`,
        `kutner2005linear`, `montgomery2017design`, `kohavi2020trustworthy`.
      - **Slides `Aulas2026/` — citações reforçadas em todos os 14 decks**: MATD48-05/06/14
        saíram de **zero** citações para 3-4 cada; os demais que estavam fracos (02, 04, 08, 10,
        11, 13) também ganharam citações adicionais, sempre cruzando com o que o capítulo do livro
        correspondente já cita. Nenhum deck precisou de slide de referências novo (todos já
        tinham).
      - **Fechamento manual (eu, direto, após os 3 agentes)**: adicionadas 7 referências clássicas
        que os agentes sinalizaram como faltantes mas não fabricaram — `bonferroni1936`,
        `shapirowilk1965`, `levene1960`, `bartlett1937`, `boxcox1964`, `boxwilson1951` (artigo
        original de superfície de resposta), `yates1933latin` (aleatorização de quadrado latino,
        antes citada só por ano em prosa) — a `Livro/refs.bib` e `Aulas/refs.bib`, e inseridas nos
        pontos exatos já identificados pelos agentes em `03-dca.Rmd`, `04-blocos.Rmd`,
        `06-fatoriais-avancado.Rmd`, `Aulas2026/MATD48-06/07/11.Rmd`.
      - **Verificação de integração completa**: os 3 agentes trabalharam em paralelo em arquivos
        disjuntos (Cap.7 novo / Cap.1-6 / Aulas2026), cada um verificando apenas render standalone
        para evitar a corrida conhecida de `bookdown::render_book()` concorrente; eu fiz a
        integração final (`rm -rf _bookdown_files && bookdown::render_book()`) duas vezes — uma
        logo após os 3 agentes terminarem, outra depois das minhas próprias correções de
        referência — ambas exit 0, livro completo com 7 capítulos navegáveis
        (`Livro/ab-bandits.html` confirmado no HTML gerado). Nada commitado ainda.
- [x] **5ª rodada**: professor pediu diagramas de Hasse e o elo amostragem↔desenho experimental,
      mais gráficos no livro e nos slides. Introduzi eu mesmo, diretamente (conteúdo fundacional,
      precisa de notação/estilo consistente): `Livro/hasse_helpers.R` (função `plot_hasse()`
      reutilizável em ggplot2, `source()`ada por cada capítulo — bookdown roda `new_session: yes`,
      então não há como compartilhar objeto de sessão entre capítulos); nova seção `Livro/
      02-modelos-lineares.Rmd` "## Diagramas de Hasse: a estrutura do delineamento antes da
      álgebra {#hasse}" com dois exemplos (DCA em cadeia; fatorial A×B cruzado), regra de contagem
      de gl por subtração, ligada à teoria de posto/matriz de projeção já existente no capítulo;
      nova seção `Livro/01-principios.Rmd` "### O elo entre amostragem e planejamento de
      experimentos {#amostragem-e-desenho}" distinguindo as duas aleatorizações (amostragem = quem
      entra no estudo/validade externa; atribuição = quem recebe qual tratamento/validade interna),
      amarrando à fórmula de variância de Neyman já presente e ao submuestreo do Cap.3. Referências
      novas: `bailey2008design` (Hasse em DOE), `kish1965survey` (amostragem).
      - 3 agentes em paralelo estenderam o padrão: (a) Cap.5 ganhou diagrama de Hasse do fatorial
        A×B×C (8 nós, gl somam 54=N, verificado); Cap.6 ganhou diagrama de Hasse **visualizando
        confusão** (2 painéis lado a lado, Bloco e ABCD disputando o mesmo 1 gl) + gráfico de
        barras dos 15 efeitos do 2⁴ replicado que faltava; (b) Cap.3 ganhou diagramas de Hasse do
        DCA de distração (3 nós) e do submuestreo mojarra (4 nós, cadeia aninhada, gl somam
        100=N); Cap.4 ganhou diagramas do DBCA pepino (4 nós cruzados sem interação, gl somam
        24=N) e do quadrado latino 4×4 (5 nós, gl somam 16=N); (c) 8 decks de `Aulas2026`
        (03/04/05/07/08/09/11/12) ganharam diagrama de Hasse (03/04/05/09/11/12) e/ou reforço
        gráfico geral (07/08) — os agentes (a) e (c) bateram no limite de sessão da conta antes da
        verificação final própria, mas o trabalho de edição já estava completo; eu verifiquei
        pessoalmente: os 10 arquivos tocados (2 capítulos + 8 decks) renderizam exit 0, e conferi
        manualmente a soma dos graus de liberdade de cada diagrama de Hasse novo contra o N do
        exemplo (todos batem exatamente). Build de integração completa do livro (7 capítulos)
        depois de tudo: exit 0. Nada commitado ainda.
- [x] **6ª rodada — auditoria severa de renderização** (professor: "fala de figura isto figura
      aquilo mas não aparecem as figuras renderizadas... fórmulas de LaTeX que aparecem
      explicitamente mas não renderizadas"). "Exit 0" nas rodadas anteriores garantia só que
      pandoc/knitr não travavam — nunca confirmava visualmente que o HTML publicado estava
      correto. Auditoria encontrou e corrigiu **dois bugs reais e sistêmicos**, cobrindo o livro
      inteiro (não só o material desta sessão):
      1. **Imagens geradas por R quebradas em todo o livro** (55 de 63 `<img>` — só as poucas
         reaproveitadas de `Aulas/images/` funcionavam): com `output_dir: "."`, o
         `bookdown::render_book()` deixa os PNGs de cada capítulo só dentro de
         `_bookdown_files/0N-capitulo_files/figure-html/`, mas as páginas HTML finais linkam para
         `0N-capitulo_files/figure-html/...` (sem o prefixo `_bookdown_files/`) — reproduzido de
         forma determinística mesmo numa build 100% limpa (sem cache, sem `.md` residual). Corrigido
         copiando manualmente `_bookdown_files/*_files` para a raiz de `Livro/` após cada build;
         documentado como "Known footgun #2" em `CLAUDE.md`, com o comando exato a rodar sempre
         após `render_book()`. Confirmado: 63/63 imagens resolvem agora.
      2. **`\@ref(...)` quebrado (aparecendo como texto cru "Figura \@ref(fig:xyz)") em 13 pontos**
         do livro + 1 nos slides — três causas-raiz distintas, todas fora do mecanismo normal de
         resolução de referências do bookdown: (a) dentro de `fig.cap="..."` referenciando OUTRA
         figura (6 casos, principalmente nas legendas dos diagramas de Hasse novos); (b) dentro de
         comentário de código R (4 casos, `Livro/06-fatoriais-avancado.Rmd`, de uma rodada
         anterior); (c) dentro de bloco raw ```` ```{=html} ```` — as caixas
         `caixa-aplicacao`/`caixa-discussao` usadas no livro inteiro (4 casos, em
         `05-fatoriais.Rmd`, `06-fatoriais-avancado.Rmd`, `07-ab-testing-bandits.Rmd`). Todos os 13
         reescritos como texto simples (sem `\@ref`), preservando o sentido. Mais 1 caso em
         `Aulas2026/MATD48-12.Rmd`: `\@ref()` usado para referenciar o livro a partir de um slide
         xaringan standalone, onde bookdown nunca resolve nada — também corrigido. Documentado como
         "Known footgun #3" em `CLAUDE.md`. Confirmado por varredura Python precisa (rastreando
         estado dentro/fora de chunk R, comentário, `fig.cap`, bloco html) em todos os `Livro/0*.Rmd`:
         zero ocorrências problemáticas restantes; confirmado também no HTML final (`grep -l
         '@ref(' *.html` vazio).
      3. Durante a correção, uma edição minha introduziu um bug novo (aspas duplas aninhadas dentro
         de uma string `fig.cap="..."`, quebrando o parse do cabeçalho do chunk) — pego na hora
         porque *rodei* a build limpa em vez de só editar às cegas; corrigido antes de prosseguir.
      4. Verificação adicional (sem bug encontrado): MathJax carrega corretamente (mecanismo padrão
         do bookdown, injetado via JS, precisa de internet no navegador — comportamento normal, não
         um bug introduzido aqui); fórmulas dentro de `caption=` de `kable()` renderizam
         corretamente como `<span class="math inline">`; `Aulas2026/` não tem o bug #1 (xaringan
         gera `MATD48-NN_files/figure-html/` direto, sem camada de cache `_bookdown_files`,
         confirmado por checagem de imagens); `Projetos/` e `Listas2026/` sem `\ref`/`\@ref`
         quebrado (não usam esse mecanismo).
      5. **Revisão de ordem lógica do livro** (pedido explícito do professor): extraí o sumário
         completo (todos os `#`/`##`/`###`) dos 7 capítulos e revisei a sequência. Conclusão: a
         ordem já é coerente e didaticamente sólida — bate com a progressão de tópicos da Luz
         capítulo a capítulo (confirmado contra o sumário dela extraído em rodada anterior) e com
         Owen para o Cap.7; dentro de cada capítulo, pré-requisitos vêm antes de quem os usa (ex.:
         "não replicado" antes de "confusão", que depende de raciocinar com réplica única). Único
         ponto de atenção verificado com cuidado: diagramas de Hasse abrem o Cap.2, antes da
         álgebra formal — decisão deliberada (título da seção já diz "antes da álgebra"), casada
         com a abordagem do próprio Bailey (2008) citado, com referência futura sinalizada
         explicitamente ("adiante"). Não reestruturei nada — nenhum problema real de sequência
         encontrado.
      - Full rebuild final verificado: 63/63 imagens ok, 0 `\@ref` quebrados, exit 0. Nada
        commitado ainda.
- [x] **7ª rodada** (feedback: refs sumidas nos slides, gráfico prova A/B não distinguia A de B
      "erros dessa natureza permanecem ao longo do livro", pedido de mais conexão
      variabilidade↔aleatorização↔forma do modelo, pedido de capítulo(s) novo(s) para quadrados
      latinos/greco-latinos e superfície de resposta, imagens em inglês).
      - **Bug real e sistêmico encontrado por reprodução mínima**: `xaringan::moon_reader` nunca
        processa citação pandoc (`[@chave]`) nem `<div id="refs">` — confirmado isolando um .Rmd
        de 5 linhas fora do repo, mesmo forçando `pandoc_args: ["--citeproc"]` manualmente.
        Causa: xaringan entrega markdown quase cru para uma `<textarea>` renderizada client-side
        pelo remark.js, sem passar pelo pipeline pandoc completo. Afeta os 14 decks desde o
        início do projeto — a auditoria de citações de uma rodada anterior só conferia exit code,
        nunca abriu o HTML gerado. Agente disparado para reescrever `[@chave]`→"(Autor, Ano)" à
        mão e reconstruir cada slide de Referências como lista markdown manual, com checagem
        textual pós-render (não só exit 0) para confirmar.
      - **Gráfico `plot-submuestreo` (Cap.1 + `Aulas2026/MATD48-01.Rmd`) corrigido**: mapeava só
        `color=tecnica`, sem nenhuma estética para `prova` (A/B) — exatamente o erro que o
        professor apontou ("deveríamos ter a capacidade de distinguir a prova A e B"). Adicionado
        `shape=prova` com `scale_shape_manual`; verificado visualmente (li o PNG gerado) que A/B
        agora aparecem como círculo/triângulo. Também corrigido um subtítulo do slide que dizia
        "cada cor um estudante" quando o código colore por técnica.
      - **Imagem em inglês trocada**: `there_is_only_one_test.png` (diagrama de Allen Downey, só
        esse — auditei visualmente os outros ~9 arquivos reaproveitados de `Aulas/images/` e
        todos já estavam em português, nenhum outro precisou de troca) substituída em
        `03-dca.Rmd` por um diagrama nativo em ggplot (4 caixas, mesmo esquema, texto em
        português) + um histograma novo da distribuição de permutação (`F_perm`) real do exemplo
        do capítulo sobreposta à densidade $F(2,33)$ teórica — mais forte pedagogicamente que a
        imagem genérica que substituiu, e já atende em parte o pedido de mais conexão
        variabilidade↔aleatorização.
      - **Restruturação: superfície de resposta virou Capítulo 7 dedicado** (extraído do antigo
        Cap.6), renumerando testes A/B/bandits para Capítulo 8. Decisão consciente: quadrados
        latinos/greco-latinos **não** foram extraídos do Cap.4 para não arriscar uma segunda
        cascata de renumeração no meio do livro — ficam aprofundados no lugar. `_bookdown.yml`,
        `Livro/index.Rmd` (tabela de capítulos) e a home do curso (contagem de capítulos,
        cronograma aula 14→Cap.6+Cap.7) atualizados; verificado 0 imagens quebradas, 0 `\@ref`
        quebrados após a divisão.
      - 3 agentes em paralelo disparados: (a) reescrever citações dos 14 decks de `Aulas2026/`;
        (b) auditar Cap.1-3 por bugs gráficos do mesmo tipo do prova A/B + aprofundar
        variabilidade↔aleatorização↔forma do modelo; (c) aprofundar quadrados
        latinos/Youden/MOLS no Cap.4 + preencher os TODOs do Cap.7 novo (CCD, análise canônica,
        steepest ascent, desejabilidade, ridge). Instruídos a verificar visualmente (ler o PNG)
        cada gráfico, não só conferir exit 0 — lição direta desta rodada.
      - **Agente (a) concluído — citações Aulas2026**: as 34 chaves citadas nos 14 decks existiam
        em `Aulas/refs.bib` (nenhuma órfã). Todo `[@chave]` virou texto formatado à mão
        ("Fisher, 1935"), todo `<div id="refs">` virou lista markdown manual com as referências
        completas do deck. Checagem textual pós-render (não só exit 0) confirmou 0 citações
        quebradas sobrando nos 14 HTMLs — verifiquei por amostragem eu mesmo (MATD48-01.html) e
        bate.
      - **Agente (b) concluído — auditoria gráfica + aprofundamento Cap.1-3**: releu todos os
        ggplot() de Cap.1-3; não achou nenhuma outra instância do bug prova A/B (confirmou
        inclusive um caso sutil — IDs de aquário em `mojarra.csv` não colidem entre grupos).
        Adicionou 3 seções novas conectando fontes de variabilidade → aleatorização → forma do
        modelo: `#aditividade-aleatorizacao` (Cap.1, por que a decomposição força a forma
        aditiva), `#randomizacao-df-f` (Cap.3, de onde vêm os gl (2,33) da F — com chunk novo que
        recalcula via `qr(model.matrix())$rank` e confere contra os gl já usados no gráfico
        `plot-f-perm`), `#porque-xbeta-mais-erro` (Cap.2, por que Y=Xβ+ε é a forma natural, não
        arbitrária). Verificado por mim: as 3 seções existem, os 3 capítulos renderizam exit 0.
      - **Agente (c) — Cap.4 concluído, Cap.7 estagnou (timeout de 600s sem progresso) e foi
        terminado por mim diretamente.** Cap.4: seções novas "Eficiência relativa do quadrado
        latino frente ao DBCA e ao DCA", segundo exemplo completo (5 variantes de checkout em
        e-commerce, com IC ajustado por `emmeans`), "Quadrados de Youden" (heatmap 7×3, análise
        completa) e "MOLS: existência e o problema de Euler" (construção via GF(n), 4 MOLS de
        ordem 5 verificados numericamente em R, história de Tarry 1900/Bose-Shrikhande-Parker
        1959-60 com as duas referências novas adicionadas a `refs.bib` — o agente corretamente não
        as inventou). Corrigi um título de gráfico cortado (Youden) e adicionei as citações
        Tarry/BSP nos 3 pontos que o agente tinha deixado como texto plano.
        **Cap.7 (RSM) completei eu mesmo** (o agente nunca chegou a essa parte): análise canônica
        completa (autovetores da Hessiana, forma $\hat y=\hat y_0+\lambda_1w_1^2+\lambda_2w_2^2$),
        análise de ridge (recodifiquei Velocidad/ângulo para unidades comparáveis antes — um raio
        euclidiano em unidades brutas não fazia sentido físico), caminho de máxima inclinação
        (exemplo novo de secagem de fruta, 2 fatores) e CCD rotacionável ($\alpha=(2^k)^{1/4}$,
        13 corridas) com otimização multi-resposta por desejabilidade (Derringer-Suich). **Três
        bugs pegos por verificação visual antes de fechar** (não só exit 0): (1) `uniroot()` da
        análise de ridge falhava por instabilidade numérica perto da singularidade — troquei por
        busca em grade de ângulo + refinamento; (2) o caminho de ridge resultante tinha um salto
        descontínuo porque `optimize()` assume unimodalidade e a forma quadrática indefinida sobre
        um círculo tem 2 mínimos — troquei por busca em grade fina (720 pontos) antes do
        refinamento; (3) o gráfico do caminho de máxima inclinação alegava no texto "sobe até o
        passo 4 e cai" mas a única codificação visual (cor) não deixava isso perceptível (a escala
        de cor é dominada pela subida inicial) — adicionei um segundo painel (retenção vs. passo)
        que torna o pico inequívoco, e reduzi o ruído da simulação para o efeito não ficar mascarado
        por uma única corrida ruidosa por passo.
      - **Verificação de integração final**: `rm -rf _bookdown_files *.md 0*_files` +
        `bookdown::render_book()` do zero, cópia dos diretórios de figura de `_bookdown_files/`
        para a raiz (footgun #2 documentado em `CLAUDE.md`) — 70/70 imagens resolvendo (63→70,
        todos os gráficos novos desta rodada incluídos), 0 `\@ref` quebrados, exit 0. Nada
        commitado ainda.

- **2026-08-24 — Calendário 2026.2 confirmado pelo professor, mudança de sistema de avaliação,
  gabaritos retirados do repositório.**
  - **Calendário:** aulas passam a começar **25/08/2026** (terça, confirmado pelo professor —
    substitui a data tentativa de 01/09), **sem aula em 10/09/2026** (a Aula 03 passa a ocupar
    08/09 e 15/09), e o curso encerra em **18/12/2026** (últimas aulas/reserva em 15 e 17/12).
    Sala unificada para as duas aulas semanais: **Sala 121, PAF I**, terça e quinta, 10:40–12:30
    (antes: Sala 208/PAF1 na terça, Lab 140/IME na quinta). O cronograma completo em `index.Rmd`
    foi recalculado sessão a sessão para acomodar essas três restrições mantendo a ordem e o
    conteúdo das 14 aulas.
  - **Sistema de avaliação mudou de novo**: cada nota parcial $N_k$ ($k=1,2,3$) volta a ter uma
    prova escrita, agora **$N_k = 0{,}4\times\text{Projeto}_k + 0{,}6\times\text{Prova escrita}_k$**
    (antes: 50% projeto + 50% média das listas do período). As listas semanais continuam existindo
    e alinhadas 1:1 às aulas, mas passam a ser **material de prática, sem valer nota** — deixaram de
    entrar na composição de qualquer nota parcial. Datas de prova escrita/fechamento de cada
    período (mesma data da entrega do projeto): N1 22/10/2026, N2 17/11/2026 (antes 19/11), N3
    10/12/2026 (antes 17/12).
  - **Gabaritos (`Listas2026/Gabarito*.pdf`/`.tex`) removidos do repositório** e movidos para
    `/home/raydonal/Github/Cursos/matd48-gabaritos-privados/` (pasta irmã, fora do repositório git
    — nunca chega ao GitHub Pages). Removidos via `git rm`/`mv`, não apenas gitignorados, porque já
    estavam commitados (commit `e55ed69`). `index.Rmd` não linka mais gabarito nenhum na coluna
    "Material" do cronograma. `Lista*.pdf`/`.tex` permanecem em `Listas2026/` normalmente.
  - **Verificação pós-mudança**: `index.Rmd` re-renderizado (`rmarkdown::render`, exit 0); checagem
    de links locais do `index.html` (0 quebrados de 76); `Livro/` (0 imagens quebradas de 70, 0
    `\@ref` crus) e `Aulas2026/` (0 citações cruas) conferidos sem re-render — nenhum dos dois teve
    conteúdo alterado nesta rodada, só `index.Rmd` e a pasta `Listas2026/`. Também corrigido um bug
    pequeno pré-existente em `index.Rmd`: os links de Rstudio/TeXnicCenter na seção "Software"
    estavam sem `https://`, resolvendo como link local quebrado em vez de link externo.

- **2026-09-19 — 8ª rodada (piloto), após nova correção do professor**: "slides deste ano ainda
  superficiais frente a 2025 e ao material de Cristiano (MES935), aula de 1h40 recebendo slide de
  50 min, livro com equações/gráficos com problema, exemplos sem solução clara — corrigir tudo de
  novo, com rigor, incluindo Listas2026 e Projetos". Escopo grande demais para uma passada só;
  segui o padrão da Rodada 1 (piloto define o padrão antes de replicar) e parei ao fim deste módulo
  para o professor revisar antes de continuar aos 4 módulos restantes.
  - **Achado mecânico, corrigido antes de qualquer conteúdo**: o HTML do livro já commitado tinha
    **53 de 71 imagens quebradas** (footgun #2 do `CLAUDE.md` recorrendo — `render_book()` rodado
    sem o passo de copiar `_bookdown_files/*_files` para a raiz). Corrigido com rebuild limpo +
    cópia; sozinho, resolve boa parte da queixa "figura não aparece".
  - **Fontes novas mineradas**: `cristiano/` (curso de pós MES935, nunca usado antes neste
    projeto) — datasets reais (`feijao.txt`, DCA de fertilizante em feijão) e código de
    half-normal plot para efeitos fatoriais, reservados para os módulos DCA e Fatoriais (Fase 2,
    ainda não feita); `Aulas/MATD48-01.Rmd` (2025) re-minerado slide a slide contra
    `Aulas2026/MATD48-01.Rmd`.
  - **Aula 01** (27→49 slides, verificado sem transbordo por `scripts/verificar_slides.R`,
    0 imagens quebradas, 0 citações cruas): adicionadas vinhetas históricas de van Helmont
    (quantificação) e Priestley (controle) como degraus antes de Fisher/tabela-de-chá — reaproveita
    `Aulas/images/Van_Helmont_Experiment.jpg` e `priestly.jpg`, já existentes; slide de ponte QPDAC
    entre Bloco 1 e 2; segundo exemplo completo (Question→Plan→Data→Analysis→Conclusion) num
    terceiro domínio — teste A/B de layout/conversão em ciência de dados, com resultados
    potenciais binários, `prop.test()`, gráfico de barras com IC, e interpretação em linguagem
    simples — mais uma tabela final comparando os dois exemplos lado a lado (mesmos ingredientes,
    domínios diferentes).
  - **Cap. 1**: nota histórica de Fisher/Rothamsted expandida com van Helmont/Priestley (mesma
    dupla da aula, prosa em vez de slides); nova seção "Um terceiro domínio: resultados potenciais
    binários em um teste A/B" (mesmo exemplo A/B da aula, com o mesmo dataset simulado) logo após a
    fórmula de variância de Neyman — o capítulo tinha os domínios psicologia e agricultura mas
    nenhum de ciência de dados apesar de ser um dos três eixos do curso.
  - **Bug real pego por verificação visual do HTML** (não só exit 0, lição das Rodadas 6-7): a
    string condicional do p-valor produzia `p < 0{,}001` como texto cru fora de modo matemático
    (chaves LaTeX aparecendo literalmente na página) — corrigido envolvendo em `$...$`; mais dois
    números inline fora de modo matemático usando ponto decimal em prosa PT-BR (`1.4 pontos
    percentuais`) — corrigidos com o mesmo padrão já usado no resto do livro (números computados
    dentro de `$...$`, mesmo com ponto decimal, é o padrão estabelecido nos Caps. 3-4).
  - **Verificação final**: rebuild limpo do livro (`rm -rf _bookdown_files` + `render_book()`) —
    72/72 imagens resolvendo, 0 `\@ref` quebrados; deck 01 renderiza exit 0, 0 transbordo, 0
    citação crua. Nada commitado ainda (sem `.git` nesta cópia de trabalho — ver `CLAUDE.md`).
  - **Pendente** (Fase 2 do plano, aguardando aprovação do professor neste piloto): módulos
    Modelos Lineares (Cap.2, Aulas 02-03), DCA (Cap.3, Aulas 04-08, com `feijao.txt` de Cristiano),
    Blocos (Cap.4, Aulas 09-11), Fatoriais (Cap.5-6, Aulas 12-14, com half-normal de Cristiano);
    depois `Listas2026/` e `Projetos/`.

- **2026-09-19 (cont.) — correções pontuais reportadas pelo professor + 1ª mineração real de
  Cristiano**, antes de retomar a Fase 2 módulo a módulo:
  - **Bug real de layout, `Aulas2026/MATD48-03.Rmd` (Hasse)**: a equação de exibição
    `$$gl(v)=\dots$$` dentro de `.pull-left[` estourava a largura da coluna e invadia
    `.pull-right[`; e o `.footnote[...]` (absolutamente posicionado por padrão do remark.js,
    portanto invisível ao script `verificar_slides.R`, que ignora `position:absolute`) colidia com
    a legenda do diagrama. Causa-raiz identificada por screenshot real (Chrome headless via
    `chromote`, não só a métrica de transbordo) — MathJax renderiza o somatório com limites bem
    mais alto/largo que o texto ao redor. Corrigido: equação movida para fora das colunas (largura
    plena), SVG reduzido (`tamanho` 3.0×3.3in → 2.2×2.4in), footnote convertido em texto comum no
    fluxo (não `.footnote[]`). Verificado: 0 transbordo, 0 sobreposição, screenshot conferido
    visualmente após cada tentativa (3 iterações até fechar).
  - **Imagem em inglês esquecida em `Aulas2026/MATD48-04.Rmd`**: `there_is_only_one_test.png`
    (Allen Downey) — a Rodada 7 já tinha trocado essa mesma imagem no livro (`03-dca.Rmd`) por um
    diagrama `ggplot` nativo em português, mas nunca atualizou o slide correspondente, que ainda
    linkava a imagem antiga. Corrigido reaproveitando o código `ggplot` exato do livro (4 caixas:
    dados→estatística T*→mecanismo H0→reembaralhamentos→distribuição de referência). Confirmado:
    nenhuma outra referência a essa imagem resta no repositório.
  - **Varredura de equações `$$...$$`**: auditados todos os blocos de exibição em MATD48-02/03/04
    (screenshot de cada slide com equação complexa — `\begin{cases}`, `\underbrace{}`, somatórios)
    após o fix do Hasse; nenhum outro erro de renderização encontrado nesses três decks.
  - **1ª mineração real de `cristiano/` (MES935)**: extraído texto de `mes935-parte2.pdf` via
    `pdftotext` — confirma o conceito de **população conceitual de respostas** (mesma estrutura dos
    resultados potenciais/tabela científica do Cap.1, vocabulário clássico de Kempthorne em vez do
    vocabulário causal de Neyman-Rubin) e que diagramas de Hasse remontam a Throckmorton (1961),
    tese de Iowa State, sistematizada depois por Bailey (2008) — citação verificada via busca web
    antes de adicionar (nunca fabricada). Adicionado: nova Seção 2.1 em `02-modelos-lineares.Rmd`
    ("Da população conceitual de respostas ao modelo linear", citando @kempthorne1952design e
    @hinkelmann2008design, ligada por `\@ref` à tabela científica e à Seção Neyman-Rubin do Cap.1);
    citação de Throckmorton adicionada ao lado de Bailey na abertura da Seção de Hasse; espelhado
    nos slides — `Aulas2026/MATD48-02.Rmd` ganhou slide novo (mesma ponte terminológica) e
    `Aulas2026/MATD48-03.Rmd` ganhou a citação inline na sua própria slide de Hasse; `refs.bib`
    (Livro e Aulas) e as duas listas de Referências dos decks atualizadas. `feijao.txt` e o
    half-normal de Cristiano ainda não usados — ficam para os módulos DCA/Fatoriais na Fase 2.
  - **Verificação**: rebuild completo do livro (72/72 imagens, 0 `\@ref` quebrados, citações novas
    resolvendo corretamente — conferido no HTML gerado); `verificar_slides.R` limpo (0 transbordo)
    em MATD48-02/03/04; 0 citações cruas `[@chave]` nos 3 decks. Nada commitado ainda.

- **2026-09-19 (cont. 2) — módulo DCA (Cap.3, Aulas 04-08, Listas 04-08)**, a pedido do professor
  ("sim pode continuar"):
  - **Varredura mecânica dos 10 decks restantes** (MATD48-05 a 14): render individual + 
    `verificar_slides.R` em todos — 0 transbordo em qualquer um; checagem de `.footnote[]`
    (risco de colisão, o mesmo bug da Rodada anterior) nos decks que usam — MATD48-07 (2×) e
    MATD48-09 (1×) — todos com folga suficiente, confirmado por screenshot; 0 citações cruas
    `[@chave]`, 0 imagens locais quebradas em nenhum dos 14 decks. Conferido também que só 4
    imagens de `Aulas/images/` seguem reaproveitadas no total (`circular_flowchart.png`,
    `priestly.jpg`, `quadro5Blocos.png`, `Van_Helmont_Experiment.jpg`) — nenhuma em inglês restante.
  - **2ª mineração real de `cristiano/`**: `feijao.txt` (DCA real, 4 fertilizantes × 5 parcelas,
    produção de feijão) — dataset genuinamente novo, nunca usado no projeto. Convertido para
    `Livro/data/feijao.csv` (mesma convenção dos outros dados reais do capítulo, ex. `mojarra.csv`).
    Adicionado como fechamento da Seção \@ref(dca-uma-via) do Cap.3 ("Um segundo exemplo, com dado
    real"): mesma máquina do exemplo simulado (distração/tempo de reação) aplicada a um dado real,
    sem efeito verdadeiro conhecido — ANOVA real ($F=9{,}21$, $p<0{,}001$), gráfico de médias com
    IC, e uma nota de "honestidade estatística" sinalizando que Shapiro-Wilk nos resíduos dá
    $p\approx0{,}046$ (normalidade no limiar), encaminhando para a Seção de pressupostos —
    verificado numericamente (rodado em R antes de escrever o texto, não estimado). Espelhado em
    `Aulas2026/MATD48-04.Rmd` (2 slides novos, mesma análise, mesmo gráfico). Confirmado: `mojarra`
    já era dado real (de `ApoioLuz/BasesDatosDE.xlsx`, ao contrário do que a rodada anterior
    presumiu) — o gap real era especificamente agricultura com desenho a uma via simples.
  - **Listas 04-08 verificadas**: as 5 compilam limpo via `latexmk`; todas já têm pelo menos uma
    questão de dedução/prova e cobertura dos 3 domínios (psicologia/agricultura/ciência de dados) —
    nenhuma mudança necessária.
  - **Achado a decidir com o professor, não implementado**: `cristiano/mes935-parte6.pdf` dedica
    uma seção inteira a **parcelas subdivididas (split-plot)** — tópico ausente do livro e dos
    slides atuais. É um desenho genuinamente novo (dois erros experimentais, dois tamanhos de
    parcela), não uma reformulação de algo já coberto. Seguindo o precedente da Rodada 4 (Cap.7/8
    tratados como "além do semestre" em vez de forçados nas 14 aulas), não adicionei nada — fica
    para decisão do professor: vira seção nova no Cap.4 (dentro do semestre, exigiria reorganizar
    Aulas 09-11) ou conteúdo "além do semestre" como o Cap.7/8.
  - **Verificação final**: rebuild completo do livro (`rm -rf _bookdown_files` + `render_book()`) —
    73/73 imagens (nova: `feijao-plot`), 0 `\@ref` quebrados; MATD48-04 renderiza exit 0, 0
    transbordo (39 slides, era 37). Nada commitado ainda.

- **2026-09-19 (cont. 3) — Split-plot adicionado ao Cap.4** (professor: "pode continuar e adiciona
  sim o split-plot"), mais varredura mecânica final:
  - **Varredura dos 6 decks restantes** (MATD48-09 a 14): render + `verificar_slides.R` — 0
    transbordo em todos; `.footnote[]` de MATD48-07 (2×) e MATD48-09 (1×) conferidos por
    screenshot, sem colisão (folga suficiente); 0 citações cruas, 0 imagens locais quebradas. Os 14
    decks de `Aulas2026/` estão mecanicamente limpos.
  - **Nova Seção 4.6 "Parcelas subdivididas (*split-plot*)"** em `04-blocos.Rmd`, marcada
    explicitamente como *"Além do programa do semestre"* (mesma convenção do Cap.7/8) — não
    corresponde a nenhuma das Aulas 09–11, não exigiu reorganizar o cronograma nem criar slide
    novo (Cap.7/8 também não têm deck correspondente, confirmado antes de decidir). Conteúdo:
    motivação (por que um fator só pode ser aleatorizado a parcelas grandes e outro a pequenas,
    origem em Rothamsted, Yates 1937), exemplo real de domínio agricultura (irrigação × variedade
    de sorgo, inspirado na estrutura do curso do Cristiano mas com prosa 100% original — nenhum
    texto-fonte copiado), modelo com dois erros experimentais, diagrama de Hasse de 7 nós (dois
    nós de Erro, um por estrato de aleatorização — verifiquei a álgebra de gl à mão antes de
    escrever o código: $1+3+2+3+6+6+27=48=N$, depois confirmado batendo exatamente com o SVG
    gerado, sem precisar corrigir nada), $\mathbb E[QM]$ mostrando por que o teste do fator de
    parcela principal *precisa* usar o erro da parcela principal (nunca o da subparcela, o erro
    clássico que um código sem `Error()` explícito cometeria), simulação em R com
    `aov(..., Error(bloco/irrigacao))` cujos graus de liberdade batem exatamente com o diagrama.
    Citações novas: `@yates1937design` (já existia no `refs.bib`) e `@montgomery2017design` (já
    existia) — nenhuma fabricada. Bullet novo no resumo do capítulo, marcado "além do programa".
  - **Bug pego na primeira verificação numérica (não exit code)**: o parágrafo de interpretação
    tinha $F$ e $p$ **hard-coded** como texto (copiados de uma simulação de teste em terminal) —
    exatamente o padrão de erro que rodadas anteriores já flagraram como recorrente neste projeto.
    Corrigido antes de fechar: reescrito para extrair os valores de `summary(mod_sp)` via `r
    round(...)` inline, então os números impressos são sempre os do objeto `mod_sp` realmente
    ajustado no capítulo, não um valor digitado à mão.
  - **Verificação final**: rebuild completo do livro — 75/75 imagens, 0 `\@ref` quebrados, 0
    legenda de tabela duplicada (footgun #8). SVG do Hasse (`figuras/hasse/split-plot.svg`)
    conferido nó a nó: 7 nós, gl exatos batendo com a derivação manual. Gráfico de interação
    (retas por variedade, uma por irrigação) conferido visualmente: quase paralelas, consistente
    com o texto (interação não significativa). Nada commitado ainda.
  - **Módulo Fatoriais (Cap.5-6) já conferido, sem gap de tópico**: `mes935-parte8.pdf`
    (confusão de fatoriais $2^k$ em blocos) e `parte9/9a.pdf` (fracionados) cobrem exatamente o que
    já existe em `confusao-2k.html`/`fatoriais-blocos.html`/`fracionados.html` — nenhum tópico
    ausente, ao contrário do split-plot. `half_normal_example.R` de Cristiano é estritamente mais
    simples que o método de Lenth já implementado (sem margem de erro formal); não incorporado.
  - **Achado a decidir com o professor (não implementado)**: `mes935-parte7a.pdf` usa o exemplo
    clássico do wafer de silício (Montgomery) para introduzir o problema de **projeto robusto**
    ("nominal-the-best": minimizar variância *e* acertar uma média-alvo simultaneamente,
    modelando $\bar y$ e $\ln(s^2)$ separadamente a partir das réplicas). O Cap.7 já **aponta**
    Taguchi/projeto robusto como tópico "sem desenvolver" (decisão deliberada de uma rodada
    anterior) — antes de desenvolver essa seção agora, prefiro confirmar com o professor, já que
    foi uma escolha consciente de escopo, não um esquecimento. Professor respondeu "continue" sem
    confirmar este item — permanece pendente, não implementado.

- **2026-09-19 (cont. 4) — Listas 09-14 e os 4 Projetos verificados**, fechando a varredura
  completa pedida na mensagem original ("listas de exercícios devem ser verificadas, os projetos
  também"):
  - **Listas 09-14**: todas compilam via `latexmk` (2-3 páginas cada); todas têm pelo menos uma
    questão de dedução/prova. Cobertura de domínio boa mas não perfeitamente 3/3 em toda lista
    (Listas 10 e 11, focadas em Friedman/BIB/quadrado latino, não têm questão explicitamente
    rotulada "ciência de dados") — não corrigido, é uma variação de ênfase por tópico, não uma
    lacuna de rigor (dedução presente, 2 de 3 domínios presentes). Confirmado: nenhuma lista
    menciona split-plot (correto — é conteúdo "além do semestre", `Listas2026/` segue estritamente
    o cronograma das 14 aulas, não deveria mesmo aparecer lá).
  - **`Projeto-I` investigado antes de mexer**: data de entrega "01/11/2023" parecia bug à
    primeira vista, mas `index.Rmd` linka esse arquivo como "Prova I (2025, referência)" — é
    intencionalmente uma prova antiga preservada como exemplo de estilo/formato, não um artefato
    do curso atual. Não alterado (alterar destruiria o propósito do arquivo).
  - **Bug real encontrado nos Projetos II, III e IV**: os três ainda diziam "N_k = 50% Projeto +
    50% média das Listas" — a fórmula de avaliação **antiga**, substituída em 2026-08-24 por
    "N_k = 40% Projeto + 60% Prova escrita" quando as listas deixaram de valer nota (registrado no
    próprio `PLANO_CONTEUDO.md`, mas nunca propagado aos 3 arquivos de projeto — exatamente o tipo
    de inconsistência entre arquivos que a auditoria original deveria pegar). Corrigido nos três;
    confirmado contra a fórmula/datas atuais em `index.Rmd` antes de escrever. Verificado: os 3
    renderizam limpo em HTML *e* PDF (PDF/artefatos `.tex` gerados só para teste, não commitados —
    a convenção do diretório é manter só `.html`).
  - **Verificação final**: com isso, a varredura completa (14 decks + livro completo Cap.1-8 +
    28 listas + 4 projetos) pedida na mensagem original está concluída. Nada commitado ainda (sem
    `.git` nesta cópia de trabalho).

- **2026-09-19 (cont. 5) — Taguchi/projeto robusto desenvolvido** (professor: "desenvolva e
  continue"):
  - **Nova Seção 7.5 "Desenho robusto: otimizando a média e a variância ao mesmo tempo"** em
    `07-superficie-resposta.Rmd`, marcada "além do programa do semestre" (mesmo padrão do
    split-plot). Conteúdo: motivação do problema de Taguchi (média-alvo *e* baixa sensibilidade a
    ruído de produção) citando @taguchi1986; a crítica estatística ao método original (arranjos
    cruzados caros, razão sinal-ruído mistura informação) citando @viningmyers1990 (Vining &
    Myers, 1990, *Journal of Quality Technology* — verificado por busca antes de citar, nunca
    citado antes neste projeto); a alternativa moderna de **duas superfícies de resposta** (média
    e log-variância) a partir de um único CCD replicado — reaproveita a mesma maquinaria de CCD já
    construída na Seção \@ref(ccd), sem inventar ferramenta nova. Exemplo simulado (engenharia:
    espessura de revestimento por deposição, temperatura×pressão, alvo 15µm) com 8 réplicas por
    ponto do CCD, ajuste dos dois modelos, mapa de calor da variância com contornos da média
    sobrepostos, e otimização por busca em grade restrita (mesmo estilo computacional já usado na
    Seção de desejabilidade). Atualizado também o ponteiro em `08-ab-testing-bandits.Rmd`, que
    antes dizia "fora do escopo" — agora aponta para a Seção 7.5 como desenvolvida.
  - **Bug real e sistêmico encontrado ao verificar visualmente o próprio gráfico novo**: o título
    do meu gráfico saiu cortado na imagem. Ao investigar se era só o meu, achei que **já existia
    antes**, em pelo menos 3 gráficos publicados do livro (`ccd-desejabilidade` neste mesmo
    capítulo, `cubo-base` no Cap.5 — título *e* subtítulo cortados —, e `energia-ridge` neste
    capítulo) — `ggplot2` não quebra título automaticamente, e qualquer título/subtítulo longo
    encostava na legenda e saía da imagem sem aviso. Corrigido nos 5 gráficos (2 novos + 3
    pré-existentes) inserindo quebra de linha manual (`\n`) nos títulos/subtítulos longos;
    verifiquei cada um abrindo o PNG gerado, não só rodando o chunk.
  - **Verificação final**: rebuild completo do livro — 76/76 imagens, 0 `\@ref` quebrados,
    citações novas resolvendo. Nada commitado ainda.
  - **Estado do projeto**: com Fatoriais já conferido (rodada anterior) e Taguchi agora
    desenvolvido, todos os itens abertos da varredura original estão fechados. Trabalho restante é
    só aprofundamento incremental por módulo, se o professor pedir.

- **2026-09-19 (cont. 6) — Somas de quadrados Tipo I/II/III** (professor: "isso pode continuar";
  achado ao minerar `Aulas/MATD48-07.Rmd` de 2025 procurando material para aprofundar a Aula 06):
  - **Gap real encontrado**: o livro só tinha uma menção de passagem ao "problema das somas de
    quadrados Tipo I vs. Tipo III" (Cap.6, ao discutir ortogonalidade do $2^k$) — nunca
    desenvolvida. `Aulas/MATD48-07.Rmd` (2025) tinha um tratamento extenso do tema (com diagramas
    de Venn em R base) logo depois de contrastes, mas usando um exemplo confuso (mistura
    ilustração de contraste-se-quebra-com-desbalanceamento, que é um problema de fator único, com
    Tipo I/II/III, que só faz sentido genuíno com ≥2 fatores). Diferente do split-plot e do
    Taguchi, este tópico **é** do escopo do semestre — pertence ao fatorial A×B do Capítulo 5
    (Aula 12), exatamente onde a tabela de ANOVA já assumia balanceamento sem dizer.
  - **Nova Seção 5.3.1** em `05-fatoriais.Rmd`: por que perder o balanceamento quebra a
    ortogonalidade de $\mathbf X$; definição dos três tipos; exemplo novo e verificado numericamente
    (teste A/B com tráfego desigual, ciência de dados: layout×desconto, 30-70 sessões por célula) —
    Tipo I muda com a ordem, Tipo II não, Tipo III muda de novo (ajusta pela interação); recomendação
    prática citando @langsrud2003 (Tipo II > III quando interação é fraca). Citações novas,
    verificadas por busca antes de adicionar: @speedhockinghackney1978 (JASA, a referência clássica
    do tema) e @langsrud2003. Espelhado em `Aulas2026/MATD48-12.Rmd` (2 slides novos, mesmo
    exemplo, mesmos números).
  - **Bug real pego no primeiro render, não depois**: usei `options(contrasts=c("contr.sum",...))`
    (exigido para o Tipo III fazer sentido) sem restaurar o valor original — como `new_session:
    yes` roda o capítulo inteiro numa única sessão R, a mudança **vazou** para um chunk
    completamente não relacionado mais adiante no mesmo capítulo (`pepino-model-matrix`), que
    quebrou com "subscript out of bounds" porque os nomes de coluna do `model.matrix()` mudam de
    convenção conforme o tipo de contraste ativo. Pego porque *rodei* o render completo em vez de
    testar o chunk isolado; corrigido salvando e restaurando `options("contrasts")` no mesmo chunk,
    imediatamente após o uso.
  - **Verificação final**: rebuild completo do livro (76/76 imagens, 0 `\@ref` quebrados, 0 legenda
    duplicada) e do deck MATD48-12 (43 slides, 0 transbordo, 0 citação crua, 0 imagem quebrada).
    Nada commitado ainda.
  - **Pendente**: os decks ainda mais curtos que a média (05, 06, 08, 10, 13, 14, entre 25-31
    slides) não passaram pelo mesmo tipo de expansão de conteúdo que a Aula 01 e o início do
    módulo DCA já receberam — ficou só a varredura mecânica (bugs) e, para 12, este acréscimo de
    conteúdo. Aprofundar a densidade desses 6 decks para uma aula de 100 min é o próximo item
    natural, se o professor quiser continuar nessa direção.

- **2026-09-19 (cont. 7) — Aula 06 aprofundada** (professor: "sim, continuar"):
  - **Duas adições concretas**, sem precisar de novo dado externo: (a) resíduos estudentizados
    $r_i$ como régua para "resíduo grande" (o item 4 da teoria de resíduos nunca tinha uma régua
    numérica) — exemplo com um ponto discrepante deliberado, detectado via `rstudent()`; (b)
    fechamento do gancho deixado em aberto na Aula 04 (Shapiro-Wilk do feijão, $p\approx0{,}046$,
    "no limiar de 0,05"): Box-Cox no mesmo dado real dá $\lambda$ ótimo $\approx1{,}71$ com IC de
    verossimilhança de 95% incluindo 1 — Box-Cox discorda do veredito pontual do Shapiro-Wilk, e a
    decisão defensável é **não transformar**. Lição explícita: com $n$ pequeno, olhar o IC de
    $\lambda$ em vez de reagir a um único $p$ perto de 0,05.
  - **Verificação**: 25→31 slides, 0 transbordo (1 slide dividido em dois após detectado por
    `verificar_slides.R`), 0 citação crua, 0 imagem quebrada. Nada commitado ainda.
  - Restam 05, 08, 10, 13, 14 no mesmo estado (varredura mecânica só, sem aprofundamento de
    conteúdo).

- **2026-09-19 (cont. 8) — Aula 05 aprofundada + espelhado no livro** (professor: "sim,
  continuar"):
  - **Fechei um gancho que a própria Aula 05 já tinha aberto**: o texto dizia "a estimação correta
    passa por GLS / lme4, nlme" mas nunca mostrava — nem o motivo de precisar, na prática. Testei
    em R antes de escrever: `aov(y ~ trat + Error(trat:ue))` num desenho de submuestreo
    desbalanceado (uma UE com 2 submuestras em vez de 4) **emite o aviso real**
    `"Error() model is singular"` — não é hipotético, é o comportamento verdadeiro do R. Dois
    slides novos: o aviso do `aov()` (com `warning=TRUE` no chunk para o aviso aparecer
    renderizado, não só no console), depois `lme4::lmer()` resolvendo sem aviso, com
    `VarCorr()`/`anova()` reais (não inventados).
  - **Erro pego antes de publicar, não depois**: escrevi de cabeça "Aula 09 volta a isso com
    Kenward-Roger" — busquei no repositório inteiro antes de deixar passar e confirmei que
    **nenhum outro arquivo menciona Kenward-Roger** — teria sido uma referência cruzada fabricada,
    exatamente o tipo de erro que a Rodada 7 já tinha corrigido uma vez (referência a conteúdo que
    não existe). Reescrito para descrever o fato (exige aproximação de gl) sem apontar para um
    lugar que não existe.
  - **Espelhado no livro** (`03-dca.Rmd`, seção `#modelo-submuestreo`): mesma demonstração,
    citando a Seção Zyskind já existente do capítulo (precisei dar um `{#zyskind-dca}` a essa
    subseção, que não tinha label ainda, para poder referenciá-la corretamente em vez de inventar
    um label).
  - **Verificação**: Aula 05, 28→31 slides, 0 transbordo, 0 citação crua, 0 imagem quebrada;
    livro, 76/76 imagens, 0 `\@ref` quebrado (incluindo o novo `\@ref(zyskind-dca)`, conferido
    resolvendo). Nada commitado ainda.
  - Restam 08, 10, 13, 14 no mesmo estado.

- **2026-09-19 (cont. 9) — Aulas 08, 10, 13, 14 aprofundadas, fechando a lista de decks curtos**
  (professor: "sim, profundidade"):
  - **Aula 08** (28→34 slides): (a) resíduos estudentizados $r_i$ como régua numérica; (b)
    demonstração de **viés de mediador em ANCOVA** — simulei um cenário em que a "covariável"
    (ansiedade) é na verdade afetada pelo tratamento (dose de cafeína), e "ajustar" por ela faz o
    efeito real da dose desaparecer/quase inverter de sinal. Responde concretamente a própria
    Pergunta 1 de Discussão da aula, que antes só existia em abstrato; (c) correção de empates
    (ties) para Kruskal-Wallis, verificada numericamente contra `kruskal.test()` — fecha um gap
    real: a **Lista 08, Questão 5** já cobrava esse exato tópico (mesma notação, $\tau_j$) e o
    deck nunca tinha ensinado.
  - **Aula 10** (31→33 slides): pós-teste de Nemenyi (1963) para Friedman, com a diferença crítica
    $q_\alpha/\sqrt2 \cdot \sqrt{t(t+1)/(6b)}$ — mas **sem** forçar um exemplo numérico, porque
    testei contra o próprio exemplo da aula e descobri que Nemenyi e o Friedman global
    **discordam** nesses dados (achado real, não hipotético): documentei isso como o próprio
    ponto pedagógico (cuidado ao interpretar pós-teste sem confirmar o omnibus primeiro) em vez de
    esconder a inconsistência atrás de um exemplo fabricado para "dar certo".
  - **Aula 13** (29→31 slides): demonstração de confusão de um efeito principal (Temperatura) em
    vez de $ABCD$, reaproveitando os dados reais do biodiesel.
    - **Bug real pego antes de publicar**: minha primeira versão afirmava que o `NA` apareceria em
      "Temperatura" — rodei o código antes de aceitar essa frase e descobri que é o termo
      **recém-adicionado** que vira `NA` (`lm()` descarta a coluna redundante mais nova, não a
      mais importante), o oposto do que eu tinha escrito de cabeça. Reescrevi a demonstração
      inteira em torno do que é **verdadeiramente correto** — comparar valores ajustados de dois
      modelos (com Temperatura vs. com "dia") e mostrar que são **idênticos**, ponto a ponto —
      independente de qual rótulo o R decide marcar como `NA`. Corrigi também uma comparação
      inicial por $R^2$ que era trivialmente 1 nos dois casos (modelo saturado) e não provava
      nada — substituída por diferença de valores ajustados.
  - **Aula 14** (28→30 slides): demonstração numérica de que "aliasado" significa que o
    coeficiente é a **soma** dos dois efeitos verdadeiros sobrepostos (A+BD), não um erro de
    arredondamento — verificada contra os geradores já definidos no próprio deck.
  - **Verificação final**: as 4 decks renderizam limpo (0 transbordo, 0 citação crua, 0 imagem
    quebrada, conferido depois de cada correção, não antes). Nada commitado ainda.
  - **Estado final**: os 14 decks de `Aulas2026/` passaram por aprofundamento de conteúdo (não só
    varredura mecânica). Trabalho de "expandir para 100 min" da mensagem original está concluído
    para todos os decks identificados como curtos.

- **2026-09-19 (cont. 10) — Investigação de "erros de fórmula" + mineração de gráficos de 2025**
  (professor: "ainda há erros na renderização de fórmulas... aproveitar mais os gráficos das aulas
  do ano passado"):
  - **Verificação rigorosa de fórmulas, não encontrei erro real**: escrevi um script que abre cada
    um dos 14 decks e das 73 páginas do livro num Chrome headless de verdade, espera o MathJax
    terminar de tipografar, e procura nós `mjx-merror` (falha real de parsing) — **zero em
    qualquer arquivo**. Também re-conferi visualmente, por screenshot, todo `$$...$$` dentro de
    `.pull-left`/`.pull-right` em todos os 14 decks (o padrão exato que causou o bug do Hasse
    numa rodada anterior) — todos renderizam limpos. Não consegui reproduzir o problema relatado;
    fica registrado para o professor apontar um slide/página específico se o erro persistir (pode
    ser cache de navegador de uma versão anterior a esta sessão).
  - **Mineração de gráficos de `Aulas/images/`**: inventariei as 108 imagens da pasta — só 8
    estavam reaproveitadas antes desta sessão (+ as 2 que adicionei nas Aulas 01/04 hoje). Duas
    tabelas clássicas do algoritmo de Yates (`dadosyates.png`, `yatesout.png`) já usadas no livro
    (Cap.6) mas nunca nos slides — adicionadas a `Aulas2026/MATD48-13.Rmd` ("O algoritmo tabular,
    antes de existir pacote estatístico" + "A tabela de sinais: todos os sete efeitos de uma
    vez"). Também conferi que a visualização de "faces opostas do cubo" que o professor lembra de
    2025 (`EfeitoTK.png`, cubo com faces coloridas por nível +/-) **já está** em
    `Aulas2026/MATD48-12.Rmd` ("Efeitos principais: faces opostas do cubo") — reimplementada com
    dado real do biodiesel, não só reaproveitada; não é um gap, já foi minerada em rodada anterior.
  - **Verificação**: MATD48-13 renderiza limpo (33 slides, 0 transbordo depois de ajustar a
    largura da primeira imagem, 0 citação crua, 0 imagem quebrada). Nada commitado ainda.
  - **Pendente**: as ~96 imagens restantes de `Aulas/images/` (analytics/dashboards de ferramentas,
    material de outras aulas — Stevens, sketchplanations, distribuições) não foram revisadas uma a
    uma; se o professor tiver em mente um gráfico específico de uma aula específica, apontar ajuda
    a não garimpar às cegas.

- **2026-09-19 (cont. 11) — achado real: `sit3.png` faltando completava uma sequência pedagógica, e
  um erro de texto pré-existente no livro** (continuando a mineração de gráficos, sem pedido novo
  do professor):
  - **Descoberta**: rastreei a origem de `experimento1.png`/`sit2.png`/`sit4.png` (já usados) até
    `Aulas/www/test.R` — na verdade um arquivo de **outro curso** (MATC65, Estatística em
    Psicologia, co-lecionado com André Leite), não MATD48; a maior parte das ~96 imagens restantes
    (Stevens, Location, Dispersion, TypesStats etc.) pertence a esse outro curso e não deveria ser
    minerada aqui. Mas as 4 imagens "Situação Experimental 1-4" (adaptadas de Hinkelmann &
    Kempthorne, 1994) são genuinamente do material de planejamento de experimentos — 3 das 4 já
    reaproveitadas (Cap.3 e Cap.4), faltava só `sit3.png`.
  - **`sit3.png` é o elo que faltava**: comparei as 4 imagens lado a lado — Situação 1 (1
    caixa/tratamento, submuestreo, **fatal**: só 3 UEs no experimento todo), Situação 2 (12 vasos
    individuais, sem submuestreo), Situação 3 (2 caixas/tratamento, submuestreo com replicação —
    **exatamente** a estrutura do exemplo real da tilápia que a Aula 05/Cap.3 já usa, só que sem
    diagrama esquemático), Situação 4 (mesma coisa que 3, mas em blocos). Adicionada ao Cap.3
    (`03-dca.Rmd`, junto das 2 já existentes) e à Aula 05, como ponte visual explícita para o
    exemplo da tilápia.
  - **Bug de texto real encontrado ao verificar as imagens com cuidado**: o texto pré-existente do
    livro (rodada anterior, não desta sessão) dizia "apenas 3 UEs **por tratamento**" para a
    Situação 1 — mas a imagem mostra **uma única caixa por tratamento**, ou seja, 3 UEs **no
    total**, uma por tratamento, não três. É uma diferença que muda a gravidade do problema: com 1
    UE/tratamento não sobra grau de liberdade nenhum para estimar erro experimental — bem mais
    grave do que "3 por tratamento" sugeria. Corrigido, com os graus de liberdade de cada situação
    agora explícitos (0, 9 e 3 gl de erro, respectivamente).
  - **Verificação**: rebuild completo do livro — 77/77 imagens, 0 `\@ref` quebrados. Aula 05
    (25→33 slides): 1 bug de layout achado e corrigido (3 imagens em `include_graphics()` sem
    `fig.show="hold"` empilham verticalmente em vez de lado a lado — 1521px de transbordo antes do
    fix), 0 transbordo depois, 0 citação crua, 0 imagem quebrada. Nada commitado ainda.
