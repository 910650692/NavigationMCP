package com.fy.navi.service.adapter.user.group;


import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.user.group.GroupRequestBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;
import java.util.Objects;

public class GroupAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(GroupAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "GroupAdapterImpl";
    private final IGroupApi mGroupApi;

    private GroupAdapter() {
        mGroupApi =  (IGroupApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public static GroupAdapter getInstance() {
        return GroupAdapterHolder.INSTANCE;
    }

    private static class GroupAdapterHolder {
        private static final GroupAdapter INSTANCE = new GroupAdapter();
    }
    public void  initService() {
        mGroupApi.initService();
    }

    public void registerCallBack(String key, GroupAdapterCallback callBack) {
        mGroupApi.registerCallBack(key, callBack);
    }

    public int executeRequest(int requestType, GroupRequestBean request) {
        return mGroupApi.executeRequest(requestType, request);
    }

    public int publishTeamInfo(TeamUploadMsgBean uploadMsg) {
        return mGroupApi.publishTeamInfo(uploadMsg);
    }

    public ArrayList<MsgPushItemBean> getTeamPushMsgMessages() {
        return mGroupApi.getTeamPushMsgMessages();
    }
}
