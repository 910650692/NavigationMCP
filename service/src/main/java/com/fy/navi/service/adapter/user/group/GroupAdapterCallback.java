package com.fy.navi.service.adapter.user.group;

import com.fy.navi.service.define.user.group.GroupResponseBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

public interface GroupAdapterCallback {

    void initService();

    // 获取队伍状态结果回调通知
    void onNotifyStatus(int errCode, long taskId, GroupResponseBean result);
    // 请求解散队伍回调通知
    void onNotifyDissolve(int errCode, long taskId, GroupResponseBean result);
    // 请求创建队伍回调通知
    void onNotifyCreateTeam(int errCode, long taskId, GroupResponseBean result);
    // 请求加入队伍回调通知
    void onNotifyJoinTeam(int errCode, long taskId, GroupResponseBean result);
    // 请求退出队伍回调通知
    void onNotifyQuitTeam(int errCode, long taskId, GroupResponseBean result);
    // 队长踢人请求回调通知
    void onNotifyKick(int errCode, long taskId, GroupResponseBean result);
    // 获取队伍信息结果回调通知
    void onNotifyInfo(int errCode, long taskId, GroupResponseBean result);
    // 请求修改队伍属性回调通知
    void onNotifyUpdate(int errCode, long taskId, GroupResponseBean result);
    // 邀请好友加入队伍回调通知
    void onNotifyInvite(int errCode, long taskId, GroupResponseBean result);
    // 请求修改队伍中的昵称回调通知
    void onNotifySetNickName(int errCode, long taskId, GroupResponseBean result);
    // 请求历史好友信息列表回调通知
    void onNotifyFriendList(int errCode, long taskId, GroupResponseBean result);
    // 转换二维码链接为图片回调通知
    void onNotifyInviteQRUrl(int errCode, long taskId, GroupResponseBean result);
    // 获取队伍状态结果回调通知
    void onNotifyUrlTranslate(int errCode, long taskId, GroupResponseBean result);
    // 组队推送消息通知
    void notifyTeamUploadResponseMessage(MsgPushItemBean msg);
    // 组队位置上报返回消息通知
    void notifyTeamUploadResponseMessage(GroupResponseBean msg);
}
