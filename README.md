# 开源的尘起时刻对战网站

## 网络机制

客户端和服务端各存储一份游戏状态，
用 WebSocket 通信
事件循环：
1. 服务端 ready，判定当前行动客户端，向客户端写入行动要求
2. 客户端选择后将 Action 和当前客户端 GameState 一并传送给服务端
3. 服务端校验 GameState，失败则向客户端同步正确 GameState 并重新询问，客户端刷新并重新建立连接
4. 校验成功，则服务用 GameState 和 Action 计算 [GameState', Logs] 并向所有客户端写入更新要求，然后回到步骤1
