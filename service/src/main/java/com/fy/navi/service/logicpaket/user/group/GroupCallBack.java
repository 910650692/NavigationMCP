package com.fy.navi.service.logicpaket.user.group;

import com.fy.navi.service.define.user.group.GroupResponseBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

public interface GroupCallBack {

    void onNotifyStatus(int errCode, long taskId, GroupResponseBean result);

    void onNotifyDissolve(int errCode, long taskId, GroupResponseBean result);

    void onNotifyCreateTeam(int errCode, long taskId, GroupResponseBean result);

    void onNotifyJoinTeam(int errCode, long taskId, GroupResponseBean result);

    void onNotifyQuitTeam(int errCode, long taskId, GroupResponseBean result);

    void onNotifyKick(int errCode, long taskId, GroupResponseBean result);

    void onNotifyInfo(int errCode, long taskId, GroupResponseBean result);

    void onNotifyUpdate(int errCode, long taskId, GroupResponseBean result);

    void onNotifyInvite(int errCode, long taskId, GroupResponseBean result);

    void onNotifySetNickName(int errCode, long taskId, GroupResponseBean result);

    void onNotifyFriendList(int errCode, long taskId, GroupResponseBean result);

    void onNotifyInviteQRUrl(int errCode, long taskId, GroupResponseBean result);

    void onNotifyUrlTranslate(int errCode, long taskId, GroupResponseBean result);

    void notifyTeamPushMessage(MsgPushItemBean msg);

    void notifyTeamUploadResponseMessage(GroupResponseBean msg);
}
