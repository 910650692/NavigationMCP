package com.fy.navi.service.adapter.user.group.bls;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DInt32;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.group.GroupService;
import com.autonavi.gbl.user.group.model.GroupDestination;
import com.autonavi.gbl.user.group.model.GroupFriend;
import com.autonavi.gbl.user.group.model.GroupRequest;
import com.autonavi.gbl.user.group.model.GroupRequestCreate;
import com.autonavi.gbl.user.group.model.GroupRequestFriendList;
import com.autonavi.gbl.user.group.model.GroupRequestInfo;
import com.autonavi.gbl.user.group.model.GroupRequestInvite;
import com.autonavi.gbl.user.group.model.GroupRequestInviteQRUrl;
import com.autonavi.gbl.user.group.model.GroupRequestJoin;
import com.autonavi.gbl.user.group.model.GroupRequestKick;
import com.autonavi.gbl.user.group.model.GroupRequestQuit;
import com.autonavi.gbl.user.group.model.GroupRequestSetNickName;
import com.autonavi.gbl.user.group.model.GroupRequestStatus;
import com.autonavi.gbl.user.group.model.GroupRequestType;
import com.autonavi.gbl.user.group.model.GroupRequestUpdate;
import com.autonavi.gbl.user.group.model.GroupRequestUrlTranslate;
import com.autonavi.gbl.user.group.model.GroupResponseCreate;
import com.autonavi.gbl.user.group.model.GroupResponseDissolve;
import com.autonavi.gbl.user.group.model.GroupResponseFriendList;
import com.autonavi.gbl.user.group.model.GroupResponseInfo;
import com.autonavi.gbl.user.group.model.GroupResponseInvite;
import com.autonavi.gbl.user.group.model.GroupResponseInviteQRUrl;
import com.autonavi.gbl.user.group.model.GroupResponseJoin;
import com.autonavi.gbl.user.group.model.GroupResponseKick;
import com.autonavi.gbl.user.group.model.GroupResponseQuit;
import com.autonavi.gbl.user.group.model.GroupResponseSetNickName;
import com.autonavi.gbl.user.group.model.GroupResponseStatus;
import com.autonavi.gbl.user.group.model.GroupResponseUpdate;
import com.autonavi.gbl.user.group.model.GroupResponseUrlTranslate;
import com.autonavi.gbl.user.group.observer.IGroupServiceObserver;
import com.autonavi.gbl.user.msgpush.MsgPushService;
import com.autonavi.gbl.user.msgpush.model.TeamPushMsg;
import com.autonavi.gbl.user.msgpush.model.TeamUploadMsg;
import com.autonavi.gbl.user.msgpush.model.TeamUploadResponseMsg;
import com.autonavi.gbl.user.msgpush.observer.IMsgPushServiceObserver;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.group.GroupAdapterCallback;
import com.fy.navi.service.adapter.user.group.IGroupApi;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.group.GroupDestinationBean;
import com.fy.navi.service.define.user.group.GroupMemberBean;
import com.fy.navi.service.define.user.group.GroupRequestBean;
import com.fy.navi.service.define.user.group.GroupResponseBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.group.TeamUploadMsgBean;

import java.util.ArrayList;
import java.util.List;

public class GroupAdapterImpl implements IGroupApi, IGroupServiceObserver, IMsgPushServiceObserver {

    private static final String TAG = MapDefaultFinalTag.GROUP_SERVICE_TAG;

    private GroupService groupService;
    private MsgPushService msgPushService;
    private final List<GroupAdapterCallback> callBacks = new ArrayList<>();

    /**
     * 获取队伍状态结果回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseStatus result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        GsonUtils.copyBean(result, resultBean);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseStatus onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyStatus(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求创建队伍回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseCreate result) {

        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseCreate onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyCreateTeam(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求解散队伍回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseDissolve result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseDissolve onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyDissolve(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求加入队伍回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseJoin result) {
        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseJoin onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyJoinTeam(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求退出队伍回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseQuit result) {
        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseQuit onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyQuitTeam(errCode, taskId, resultBean);
        }
    }

    /**
     * 邀请好友加入队伍回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseInvite result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseInvite onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyInvite(errCode, taskId, resultBean);
        }
    }

    /**
     * 队长踢人请求回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseKick result) {
        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseKick onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyKick(errCode, taskId, resultBean);
        }
    }

    /**
     * 获取队伍信息结果回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseInfo result) {
        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseInfo onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyInfo(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求修改队伍属性回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseUpdate result) {
        GroupResponseBean resultBean;

        resultBean = GsonUtils.convertToT(result, GroupResponseBean.class);
        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseUpdate onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyUpdate(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求修改队伍中的昵称回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseSetNickName result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        Logger.d(TAG,"GroupResponseSetNickName onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifySetNickName(errCode, taskId, resultBean);
        }
    }

    /**
     * 请求历史好友信息列表回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseFriendList result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);

        ArrayList<GroupMemberBean> friends = new ArrayList<>();
        for (GroupFriend groupFriend : result.friends) {
            GroupMemberBean groupFriendBean = new GroupMemberBean();
            groupFriendBean.setUid(groupFriend.uid);
            groupFriendBean.setImgUrl(groupFriend.imgUrl);
            groupFriendBean.setUserName(groupFriend.userName);
            groupFriendBean.setNickName(groupFriend.nickName);
            friends.add(groupFriendBean);
        }

        Logger.d(TAG,"GroupResponseFriendList onNotify = " + resultBean.toString());

        resultBean.friends = friends;
        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyFriendList(errCode, taskId, resultBean);
        }
    }

    /**
     * 转换二维码链接为图片回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseInviteQRUrl result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);
        resultBean.setUrl(result.url);

        Logger.d(TAG,"GroupResponseInviteQRUrl onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyInviteQRUrl(errCode, taskId, resultBean);
        }
    }

    /**
     * 获取队伍状态结果回调通知
     * @param errCode 错误码
     * @param taskId task id
     * @param result 请求响应数据
     */
    @Override
    public void onNotify(int errCode, long taskId, GroupResponseUrlTranslate result) {
        GroupResponseBean resultBean = new GroupResponseBean();

        resultBean.setCode(result.code);
        resultBean.setMessage(result.message);
        resultBean.setBuffer(result.data.buffer);

        Logger.d(TAG,"GroupResponseUrlTranslate onNotify = " + resultBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.onNotifyUrlTranslate(errCode, taskId, resultBean);
        }
    }

    /**
     * 组队推送消息通知
     * @param msg 组队推送消息
     */
    @Override
    public void notifyMessage(TeamPushMsg msg) {
        MsgPushItemBean teamPushMsgBean;

        teamPushMsgBean = GsonUtils.convertToT(msg, MsgPushItemBean.class);

        Logger.d(TAG,"TeamPushMsg notifyMessage = " + teamPushMsgBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.notifyTeamUploadResponseMessage(teamPushMsgBean);
        }
    }


    /**
     * 组队位置上报返回消息通知
     * @param msg 组队位置上报返回消息
     */
    @Override
    public void notifyMessage(TeamUploadResponseMsg msg) {
        MsgPushItemBean teamPushMsgBean ;

        teamPushMsgBean =  GsonUtils.convertToT(msg, MsgPushItemBean.class);

        teamPushMsgBean.setMessageId(msg.messageId);
        teamPushMsgBean.setMessageType(msg.messageType);
        teamPushMsgBean.setStatus(msg.status);
        teamPushMsgBean.setId(msg.id);
        teamPushMsgBean.setBizType(msg.bizType);
        teamPushMsgBean.setClientId(msg.clientId);
        teamPushMsgBean.setSourceId(msg.sourceId);
        teamPushMsgBean.setUserId(msg.userId);
        teamPushMsgBean.setCreateTime(msg.createTime);
        teamPushMsgBean.setExpiration(msg.expiration);
        teamPushMsgBean.setSendTime(msg.sendTime);
        teamPushMsgBean.setText(msg.text);
        teamPushMsgBean.setTitle(msg.title);
        teamPushMsgBean.setVersion(msg.version);
        teamPushMsgBean.setAccessKey(msg.accessKey);
        teamPushMsgBean.setDeviceId(msg.deviceId);
        teamPushMsgBean.setSessionId(msg.sessionId);
        teamPushMsgBean.setReaded(msg.isReaded);
        teamPushMsgBean.setSendType(msg.sendType);
        teamPushMsgBean.setTraceId(msg.traceId);
        teamPushMsgBean.setLinkMode(msg.linkMode);

        Logger.d(TAG,"TeamUploadResponseMsg notifyMessage = " + teamPushMsgBean.toString());

        for (GroupAdapterCallback callBack : callBacks) {
            callBack.notifyTeamUploadResponseMessage(teamPushMsgBean);
        }
    }



    /**
     * 初始化服务
     */
    @Override
    public void initService() {
        Logger.d(TAG,"GroupService initSetting.");
        groupService = (GroupService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.GroupSingleServiceID);
        msgPushService = (MsgPushService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MsgPushSingleServiceID);
    }

    @Override
    public void registerCallBack(String key, GroupAdapterCallback callBack) {
        callBacks.add(callBack);
    }

    /**
     * 获取请求状态
     * @param request 请求参数
     * @return 返回错误码
     */
    @Override
    public int executeRequest(int requestType, GroupRequestBean request) {
        Logger.d(TAG,"GroupRequestStatusBean requestType = "+ requestType +" + request = " + request.toString());
        return setRequest(requestType, request);
    }

    /**
     * 发送请求指令
     * @param requestType  1，获取队伍状态  2，请求创建队伍  3，请求加入队伍 4，请求修改队伍属性 5，获取队伍信息 6，获取历史好友
     *                   7，邀请别人进群  8，队长踢人 9，修改对内昵称  10， 队伍口令分享（请求口令分享二维码链接）
     *                   11，请求转换二维码链接为图片  12，请求退出队伍
     * @param request
     * @return
     */
    private int setRequest(int requestType, GroupRequestBean request) {
        int ret = 0;
        if (requestType == 1) { // 获取队伍状态
            GroupRequestStatus result = new GroupRequestStatus();

            result.teamNumber = request.getTeamNumber();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestStatus result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 2) { // 请求创建队伍
            GroupRequestCreate result;

            result = GsonUtils.convertToT(request, GroupRequestCreate.class);
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestCreate result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 3) { // 请求加入队伍
            GroupRequestJoin result = new GroupRequestJoin();

            result.teamNumber = request.getTeamNumber();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestJoin result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 4) { // 请求修改队伍属性
            GroupRequestUpdate result;

            result = GsonUtils.convertToT(request, GroupRequestUpdate.class);
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestUpdate result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 5) { // 获取队伍信息
            GroupRequestInfo result = new GroupRequestInfo();

            result.teamId = request.getTeamId();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestInfo result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 6) { // 获取历史好友
            GroupRequestFriendList result = new GroupRequestFriendList();

            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestFriendList result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 7) { // 邀请别人进群
            GroupRequestInvite result = new GroupRequestInvite();

            GsonUtils.copyBean(request, result);
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestInvite result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 8) { // 队长踢人
            GroupRequestKick result = new GroupRequestKick();

            GsonUtils.copyBean(request, result);
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestKick result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 9) { // 修改对内昵称
            GroupRequestSetNickName result = new GroupRequestSetNickName();

            result.teamNick = request.getTeamNick();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestSetNickName result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 10) { // 请求口令分享二维码链接
            GroupRequestInviteQRUrl result = new GroupRequestInviteQRUrl();

            result.teamId = request.getTeamId();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestInviteQRUrl result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 11) { // 请求转换二维码链接为图片
            GroupRequestUrlTranslate result = new GroupRequestUrlTranslate();

            result.url = request.getUrl();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestUrlTranslate result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        } else if (requestType == 12) { // 请求退出队伍
            GroupRequestQuit result = new GroupRequestQuit();

            result.teamId = request.getTeamId();
            result.reqType = getGroupRequestType(request);
            result.taskId = request.getTaskId();

            Logger.d(TAG,"GroupRequestQuit result = " + result.toString());
            if (groupService != null) {
                ret = groupService.executeRequest(result);
            }
        }
        Logger.d(TAG,"setRequest ret = " + ret);

        return ret;
    }

    private int getGroupRequestType(GroupRequestBean request) {

        GroupRequest groupRequest = new GroupRequest();

        if (request.getReqType() == 0) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeUnknown;
        } else if (request.getReqType() == 1) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeStatus;
        } else if (request.getReqType() == 2) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeCreate;
        } else if (request.getReqType() == 3) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeDissolve;
        } else if (request.getReqType() == 4) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeJoin;
        } else if (request.getReqType() == 5) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeQuit;
        } else if (request.getReqType() == 6) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeInvite;
        } else if (request.getReqType() == 7) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeKick;
        } else if (request.getReqType() == 8) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeInfo;
        } else if (request.getReqType() == 9) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeUpdate;
        } else if (request.getReqType() == 10) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeSetNickName;
        } else if (request.getReqType() == 11) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeFriendList;
        } else if (request.getReqType() == 12) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeInviteQRUrl;
        } else if (request.getReqType() == 13) {
            groupRequest.reqType = GroupRequestType.GroupRequestTypeUrlTranslate;
        }

        groupRequest.taskId = request.getTaskId();

        return groupRequest.reqType;
    }

    /**
     *
     * @param uploadMsg 上报信息
     * @return 返回错误码
     */
    @Override
    public int publishTeamInfo(TeamUploadMsgBean uploadMsg) {
        TeamUploadMsg msg = new TeamUploadMsg();

        GsonUtils.copyBean(uploadMsg, msg);

        Logger.d(TAG,"PublishTeamInfo uploadMsg = " + uploadMsg.toString());

        if (msgPushService != null) {
            return msgPushService.publishTeamInfo(msg);
        }
        return 0;
    }

    /**
     * 获取本地历史组队推送消息
     * return 本地历史组队推送消息数据列表
     */
    @Override
    public ArrayList<MsgPushItemBean> getTeamPushMsgMessages() {
        if (msgPushService != null) {
            ArrayList<TeamPushMsg> teamPushMsgList = msgPushService.getTeamPushMsgMessages();
            ArrayList<MsgPushItemBean> teamPushMsgBeanList = new ArrayList<>();
            for (TeamPushMsg teamPushMsg : teamPushMsgList) {
                MsgPushItemBean teamPushMsgBean;
                teamPushMsgBean = GsonUtils.convertToT(teamPushMsg, MsgPushItemBean.class);
                teamPushMsgBeanList.add(teamPushMsgBean);
            }

            Logger.d(TAG,"GetTeamPushMsgMessages teamPushMsgBeanList = " + teamPushMsgBeanList.toString());

            return teamPushMsgBeanList;
        }
        return null;
    }
}
