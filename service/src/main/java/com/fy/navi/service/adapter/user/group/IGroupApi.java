package com.fy.navi.service.adapter.user.group;

import com.fy.navi.service.define.user.group.GroupRequestBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;

public interface IGroupApi {
    /**
     * 初始化服务
     */
    void initService();
    /**
     * 注册回调
     * @param key
     * @param callBack
     */
    void registerCallBack(String key, GroupAdapterCallback callBack);

    /**
     * 发送请求指令
     * @param requestType  1，获取队伍状态  2，请求创建队伍  3，请求加入队伍 4，请求修改队伍属性 5，获取队伍信息 6，获取历史好友
     *                   7，邀请别人进群  8，队长踢人 9，修改对内昵称  10， 队伍口令分享（请求口令分享二维码链接）
     *                   11，请求转换二维码链接为图片  12，请求退出队伍
     * @param request
     * @return 返回int
     */
    int executeRequest(int requestType, GroupRequestBean request);

    /**
     * 上报当前组队信息
     * @param uploadMsg
     * @return 返回int
     */
    int publishTeamInfo(TeamUploadMsgBean uploadMsg);

    /**
     * 获取本地历史组队推送消息
     * @return 返回MsgPushItemBean列表
     */
    ArrayList<MsgPushItemBean> getTeamPushMsgMessages();
}
