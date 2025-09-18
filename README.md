# ERLDA

Este projeto é uma implementação do padrão SEDA (Staged Event-Driven Architecture) em Erlang.

---

## Requisitos

- Erlang/OTP 24 ou superior
- [rebar3](https://www.rebar3.org/) 

## Instalação e Execução

1. **Clone o repositório:**
   ```sh
   git clone git@github.com:fernandoareias/erlda.git
   cd erlda
   ```
2. **Compile o projeto:**
   ```sh
   rebar3 compile
   ```
3. **Inicie o shell Erlang:**
   ```sh
   rebar3 shell
   ```
4. O servidor estará ouvindo em: `localhost:8080`

## Estrutura da pasta src/lib

A pasta `src/lib` contém módulos fundamentais para a implementação do padrão SEDA neste projeto:

- **stage_behaviour.erl**: Define o behaviour (contrato) para os estágios do SEDA, padronizando a interface e o ciclo de vida dos estágios. Permite que diferentes estágios implementem suas próprias lógicas seguindo uma estrutura comum.
- **stage_controller.erl**: Responsável por gerenciar o pool de workers de cada estágio, incluindo a criação, remoção e balanceamento dinâmico dos processos conforme a demanda do sistema.
- **stage_worker.erl**: Implementa a lógica básica de um worker, que executa as tarefas atribuídas pelo estágio. Os workers são os responsáveis pelo processamento concorrente das requisições em cada estágio.

Esses módulos são reutilizáveis e servem como base para a construção dos estágios específicos do pipeline de processamento.

## Exemplos de uso

### Servidor Web (Web Server)

Como exemplo prático de aplicação do padrão SEDA, este projeto inclui um servidor web simples, implementado inteiramente em Erlang. O servidor recebe solicitações HTTP e as processa por meio de uma pipeline de estágios, cada um responsável por uma etapa específica do processamento. Essa arquitetura demonstra como os módulos genéricos da pasta `src/lib` podem ser reutilizados para construir sistemas concorrentes, escaláveis e de fácil manutenção.

O servidor web é dividido em estágios independentes, cada um com múltiplos workers, promovendo concorrência, isolamento e escalabilidade. O fluxo de processamento de uma requisição HTTP é o seguinte:

1. **http_server**: Aceita conexões TCP usando Ranch e repassa os dados recebidos para o próximo estágio.
2. **http_server_parser_stage**: Realiza o parsing da requisição HTTP, extrai o método e caminho, e encaminha para o estágio de cache.
3. **http_server_cache_stage**: Verifica primeiro se o arquivo solicitado está em cache. Se encontrado, retorna diretamente; caso contrário, encaminha para o estágio de leitura de arquivos.
4. **http_server_get_stage**: Lê arquivos do diretório `www/` quando não estão em cache e retorna o conteúdo ou erro 404.
5. **http_server_response_writer_stage**: Formata e envia a resposta HTTP de volta ao cliente, incluindo cabeçalhos apropriados.

Cada estágio utiliza o behaviour definido em `stage_behaviour.erl` e é gerenciado pelo `stage_controller.erl`, que controla o pool de workers responsáveis pelo processamento concorrente das requisições.

### Características do Servidor Web

- **Cache Inteligente**: O `cache_stage` implementa um sistema de cache em memória usando ETS (Erlang Term Storage) para melhorar a performance ao servir arquivos frequentemente acessados.
- **Parsing Robusto**: O `parser_stage` realiza parsing completo de requisições HTTP, incluindo método, caminho e cabeçalhos.
- **Respostas HTTP Completas**: O `response_writer_stage` gera respostas HTTP válidas com cabeçalhos apropriados, incluindo data, content-type e content-length.
- **Segurança**: Implementa sanitização de caminhos para prevenir ataques de directory traversal.
- **Escalabilidade**: Usa Ranch para gerenciamento eficiente de conexões TCP com pool de acceptors configurável.

<img src="docs/image.svg" alt="Fluxo dos Estágios SEDA" width="600"/>

## Exemplo de Requisição HTTP

```sh
curl http://localhost:8080/index.html
```

A resposta será o conteúdo do arquivo `www/index.html` ou um erro 404 caso não exista.

## Teste de carga

Use o script em `load-tests/test-script.js` com o [k6](https://k6.io/):

```sh
k6 run load-tests/test-script.js
```

## Observações
- Cada estágio possui múltiplos workers, escolhidos aleatoriamente para balancear a carga.
- O projeto segue o padrão SEDA, facilitando a escalabilidade e a manutenção.
- Sinta-se à vontade para contribuir!
 
