package com.fy.navi.service.adapter.user.group;


import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.user.group.GroupRequestBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;
import java.util.Objects;

 public final class GroupAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(GroupAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "GroupAdapterImpl";
    private final IGroupApi mGroupApi;

    private GroupAdapter() {
        mGroupApi =  (IGroupApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public static GroupAdapter getInstance() {
        return GroupAdapterHolder.INSTANCE;
    }

   private final static class GroupAdapterHolder {
        private static final GroupAdapter INSTANCE = new GroupAdapter();
    }

     /**
      * 初始化服务
      */
    public void  initService() {
        mGroupApi.initService();
    }

     /**
      * 注册回调
      * @param key
      * @param callBack
      */
    public void registerCallBack(final String key, final GroupAdapterCallback callBack) {
        mGroupApi.registerCallBack(key, callBack);
    }

     /**
      * 发送请求指令
      * @param requestType
      * @param request
      * @return 返回Int型
      */
    public int executeRequest(final int requestType, final GroupRequestBean request) {
        return mGroupApi.executeRequest(requestType, request);
    }

     /**
      * 上报当前组队信息
      * @param uploadMsg
      * @return 返回Int型
      */
    public int publishTeamInfo(final TeamUploadMsgBean uploadMsg) {
        return mGroupApi.publishTeamInfo(uploadMsg);
    }

    public ArrayList<MsgPushItemBean> getTeamPushMsgMessages() {
        return mGroupApi.getTeamPushMsgMessages();
    }
}
