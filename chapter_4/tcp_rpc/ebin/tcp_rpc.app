{application,tcp_rpc,
             [{registered,[tcp_rpc]},
              {description,"tcp_rpc"},
              {vsn,"0.0.1"},
              {modules,['Elixir.TcpRpc.Server.State','Elixir.TcpRpc.Server',
                        'Elixir.TcpRpc.Supervisor','Elixir.TcpRpc']},
              {applications,[kernel,stdlib,elixir]},
              {mod,{'Elixir.TcpRpc',[]}}]}.