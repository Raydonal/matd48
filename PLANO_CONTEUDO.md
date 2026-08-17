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
