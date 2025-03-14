package com.fy.navi.service.logicpaket.user.group;

import com.fy.navi.service.adapter.user.group.GroupAdapter;
import com.fy.navi.service.adapter.user.group.GroupAdapterCallback;
import com.fy.navi.service.define.user.group.GroupRequestBean;
import com.fy.navi.service.define.user.group.GroupResponseBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;
import java.util.List;

public class GroupPackage implements GroupAdapterCallback {
    private final GroupAdapter groupAdapter;
    private final List<GroupCallBack> callBacks = new ArrayList<>();

    public GroupPackage() {
        groupAdapter = GroupAdapter.getInstance();
    }

    public static GroupPackage getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final GroupPackage sInstance = new GroupPackage();
    }

    @Override
    public void initService() {
        groupAdapter.initService();
        groupAdapter.registerCallBack("GroupPackage", this);
    }

    public void registerCallBack(String key, GroupCallBack callBack) {
        callBacks.add(callBack);
    }

    public int executeRequest(int requestType, GroupRequestBean request) {
        return groupAdapter.executeRequest(requestType, request);
    }

    public int publishTeamInfo(TeamUploadMsgBean uploadMsg) {
        return groupAdapter.publishTeamInfo(uploadMsg);
    }

    public ArrayList<MsgPushItemBean> getTeamPushMsgMessages() {
        return groupAdapter.getTeamPushMsgMessages();
    }

    @Override
    public void onNotifyStatus(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyStatus(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyDissolve(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyDissolve(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyCreateTeam(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyCreateTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyJoinTeam(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyJoinTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyQuitTeam(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyQuitTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyKick(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyKick(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInfo(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyInfo(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyUpdate(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyUpdate(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInvite(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyInvite(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifySetNickName(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifySetNickName(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyFriendList(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyFriendList(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInviteQRUrl(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyInviteQRUrl(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyUrlTranslate(int errCode, long taskId, GroupResponseBean result) {
        for (GroupCallBack callBack : callBacks) {
            callBack.onNotifyUrlTranslate(errCode, taskId, result);
        }
    }

    @Override
    public void notifyTeamUploadResponseMessage(MsgPushItemBean msg) {
        for (GroupCallBack callBack : callBacks) {
            callBack.notifyTeamPushMessage(msg);
        }
    }

    @Override
    public void notifyTeamUploadResponseMessage(GroupResponseBean msg) {
        for (GroupCallBack callBack : callBacks) {
            callBack.notifyTeamUploadResponseMessage(msg);
        }
    }
}
