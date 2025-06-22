package com.sgm.navi.service.logicpaket.user.group;

import com.sgm.navi.service.define.user.group.GroupResponseBean;
import com.sgm.navi.service.define.user.group.MsgPushItemBean;

public interface GroupCallBack {
    /**
     * 获取队伍状态结果回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyStatus(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求解散队伍回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyDissolve(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求创建队伍回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyCreateTeam(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求加入队伍回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyJoinTeam(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求退出队伍回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyQuitTeam(int errCode, long taskId, GroupResponseBean result);
    /**
     * 队长踢人请求回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyKick(int errCode, long taskId, GroupResponseBean result);
    /**
     * 获取队伍信息结果回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyInfo(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求修改队伍属性回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyUpdate(int errCode, long taskId, GroupResponseBean result);
    /**
     * 邀请好友加入队伍回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyInvite(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求修改队伍中的昵称回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifySetNickName(int errCode, long taskId, GroupResponseBean result);
    /**
     * 请求历史好友信息列表回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyFriendList(int errCode, long taskId, GroupResponseBean result);
    /**
     * 转换二维码链接为图片回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyInviteQRUrl(int errCode, long taskId, GroupResponseBean result);
    /**
     * 获取队伍状态结果回调通知
     * @param errCode
     * @param taskId
     * @param result
     */
    void onNotifyUrlTranslate(int errCode, long taskId, GroupResponseBean result);
    /**
     * notifyTeamPushMessage
     * @param msg
     */
    void notifyTeamPushMessage(MsgPushItemBean msg);
    /**
     * 组队位置上报返回消息通知
     * @param msg
     */
    void notifyTeamUploadResponseMessage(GroupResponseBean msg);
}
