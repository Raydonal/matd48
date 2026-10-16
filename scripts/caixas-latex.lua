-- caixas-latex.lua -- filtro pandoc usado por scripts/gerar_pdf_livro.R
--
-- O livro escreve as caixas (caixa-aplicacao, caixa-discussao, caixa-r, caixa-aviso,
-- caixa-preta) em blocos ```{=html}. Na saída LaTeX o pandoc DESCARTA esses blocos em silêncio.
-- Este filtro converte cada bloco HTML de caixa em Markdown (as caixas usam só strong, em,
-- code, br, sub, ol, ul, li e div), deixa o pandoc ler o Markdown -- o que preserva as
-- fórmulas $...$ -- e envolve o resultado no ambiente LaTeX \begin{caixaXXX}...\end{caixaXXX}
-- definido em scripts/livro-latex-preamble.tex. Bloco HTML que não seja caixa é descartado com
-- aviso no console, para que nada suma sem que se saiba.

local function lista(conteudo, marcador)
  conteudo = conteudo:gsub("<li[^>]*>%s*", "\n" .. marcador .. " ")
  conteudo = conteudo:gsub("%s*</li>", "")
  return "\n\n" .. conteudo .. "\n\n"
end

local function html_para_markdown(t)
  t = t:gsub("<strong>(.-)</strong>", "**%1**")
  t = t:gsub("<em>(.-)</em>", "*%1*")
  t = t:gsub("<code>(.-)</code>", "`%1`")
  t = t:gsub("<sub>(.-)</sub>", "~%1~")
  t = t:gsub("<ol[^>]*>(.-)</ol>", function(x) return lista(x, "1.") end)
  t = t:gsub("<ul[^>]*>(.-)</ul>", function(x) return lista(x, "-") end)
  t = t:gsub("<br%s*/?>%s*", "\\\n")          -- quebra de linha forçada do Markdown
  return t
end

function RawBlock(el)
  if el.format ~= "html" then return nil end
  local saida = {}
  -- um bloco pode trazer várias caixas seguidas (as caixas não se aninham)
  for classe, corpo in el.text:gmatch('<div class="(caixa%-[%w%-]+)"[^>]*>(.-)</div>') do
    local blocos = pandoc.read(html_para_markdown(corpo), "markdown").blocks
    local ambiente = classe:gsub("%-", "")      -- caixa-aplicacao -> caixaaplicacao
    table.insert(saida, pandoc.RawBlock("latex", "\\begin{" .. ambiente .. "}"))
    for _, b in ipairs(blocos) do table.insert(saida, b) end
    table.insert(saida, pandoc.RawBlock("latex", "\\end{" .. ambiente .. "}"))
  end
  if #saida == 0 and not el.text:match("^%s*<!%-%-") then
    io.stderr:write("[caixas-latex] bloco HTML sem caixa descartado: " ..
                    el.text:sub(1, 80):gsub("\n", " ") .. "\n")
  end
  return saida
end
