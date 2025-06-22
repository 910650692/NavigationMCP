package com.sgm.navi.service.logicpaket.user.group;

import com.sgm.navi.service.adapter.user.group.GroupAdapter;
import com.sgm.navi.service.adapter.user.group.GroupAdapterCallback;
import com.sgm.navi.service.define.user.group.GroupRequestBean;
import com.sgm.navi.service.define.user.group.GroupResponseBean;
import com.sgm.navi.service.define.user.group.MsgPushItemBean;
import com.sgm.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;
import java.util.List;

public class GroupPackage implements GroupAdapterCallback {
    private final GroupAdapter mGroupAdapter;
    private final List<GroupCallBack> mCallBacks = new ArrayList<>();

    public GroupPackage() {
        mGroupAdapter = GroupAdapter.getInstance();
    }

    public static GroupPackage getInstance() {
        return SInstanceHolder.SINSTANCE;
    }

    private static final class SInstanceHolder {
        static final GroupPackage SINSTANCE = new GroupPackage();
    }

    @Override
    public void initService() {
        mGroupAdapter.initService();
        mGroupAdapter.registerCallBack("GroupPackage", this);
    }

    /**
     * 注册回调
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final GroupCallBack callBack) {
        mCallBacks.add(callBack);
    }

    /**
     * 发送请求指令
     * @param requestType
     * @param request
     * @return 返回int型
     */
    public int executeRequest(final int requestType, final GroupRequestBean request) {
        return mGroupAdapter.executeRequest(requestType, request);
    }

    /**
     * 上报当前组队信息
     * @param uploadMsg
     * @return 返回int型
     */
    public int publishTeamInfo(final TeamUploadMsgBean uploadMsg) {
        return mGroupAdapter.publishTeamInfo(uploadMsg);
    }

    public ArrayList<MsgPushItemBean> getTeamPushMsgMessages() {
        return mGroupAdapter.getTeamPushMsgMessages();
    }

    @Override
    public void onNotifyStatus(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyStatus(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyDissolve(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyDissolve(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyCreateTeam(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyCreateTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyJoinTeam(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyJoinTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyQuitTeam(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyQuitTeam(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyKick(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyKick(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInfo(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyInfo(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyUpdate(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyUpdate(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInvite(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyInvite(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifySetNickName(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifySetNickName(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyFriendList(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyFriendList(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyInviteQRUrl(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyInviteQRUrl(errCode, taskId, result);
        }
    }

    @Override
    public void onNotifyUrlTranslate(final int errCode, final long taskId, final GroupResponseBean result) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.onNotifyUrlTranslate(errCode, taskId, result);
        }
    }

    @Override
    public void notifyTeamUploadResponseMessage(final MsgPushItemBean msg) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.notifyTeamPushMessage(msg);
        }
    }

    @Override
    public void notifyTeamUploadResponseMessage(final GroupResponseBean msg) {
        for (GroupCallBack callBack : mCallBacks) {
            callBack.notifyTeamUploadResponseMessage(msg);
        }
    }
}
