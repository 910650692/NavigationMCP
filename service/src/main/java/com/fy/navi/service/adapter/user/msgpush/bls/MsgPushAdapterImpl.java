package com.fy.navi.service.adapter.user.msgpush.bls;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.BLKeyValue;
import com.autonavi.gbl.aosclient.model.BLResponseBase;
import com.autonavi.gbl.aosclient.model.GAimpoiMsg;
import com.autonavi.gbl.aosclient.model.GSendToPhoneRequestParam;
import com.autonavi.gbl.aosclient.model.GSendToPhoneResponseParam;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportRequestParam;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportResponseParam;
import com.autonavi.gbl.aosclient.model.RouteDisplayPoints;
import com.autonavi.gbl.aosclient.model.RoutepathrestorationPointInfo;
import com.autonavi.gbl.aosclient.observer.ICallBackSendToPhone;
import com.autonavi.gbl.aosclient.observer.ICallBackWsTserviceInternalLinkAutoReport;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.model.UserLoginInfo;
import com.autonavi.gbl.user.msgpush.MsgPushService;
import com.autonavi.gbl.user.msgpush.model.AimPoiInfo;
import com.autonavi.gbl.user.msgpush.model.AimPoiPushMsg;
import com.autonavi.gbl.user.msgpush.model.AimRoutePushInfo;
import com.autonavi.gbl.user.msgpush.model.AimRoutePushMsg;
import com.autonavi.gbl.user.msgpush.model.AutoPushInfo;
import com.autonavi.gbl.user.msgpush.model.AutoPushMsg;
import com.autonavi.gbl.user.msgpush.model.MobileDestination;
import com.autonavi.gbl.user.msgpush.model.MobileLinkPushMsg;
import com.autonavi.gbl.user.msgpush.model.MobileLinkRequest;
import com.autonavi.gbl.user.msgpush.model.MobileRouteParam;
import com.autonavi.gbl.user.msgpush.model.MobileRouteViaPoint;
import com.autonavi.gbl.user.msgpush.model.MsgPushInitParam;
import com.autonavi.gbl.user.msgpush.model.MsgPushItem;
import com.autonavi.gbl.user.msgpush.observer.IMobileLinkObserver;
import com.autonavi.gbl.user.msgpush.observer.IMsgPushServiceObserver;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.autonavi.gbl.util.model.TaskResult;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.msgpush.IMsgPushApi;
import com.fy.navi.service.adapter.user.msgpush.MsgPushAdapterCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RoutePoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.msgpush.MobileLocation;
import com.fy.navi.service.define.user.msgpush.MobileRouteParamInfo;
import com.fy.navi.service.define.user.msgpush.MobileRouteViaPointInfo;
import com.fy.navi.service.define.user.msgpush.MobileVehicleInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;
import com.fy.navi.service.define.user.msgpush.RoutePathProjectPoints;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Objects;

public class MsgPushAdapterImpl implements IMsgPushApi, IMsgPushServiceObserver, ICallBackSendToPhone, ICallBackWsTserviceInternalLinkAutoReport, IMobileLinkObserver{

    private static final String TAG = MapDefaultFinalTag.MSG_PUSH_SERVICE_TAG;

    private MsgPushService msgPushService;
    private BLAosService mBLAosService;
    private final Hashtable<String, MsgPushAdapterCallback> callBacks;

    public MsgPushAdapterImpl() {
        callBacks = new Hashtable<>();
    }
    @Override
    public void registerCallBack(String key, MsgPushAdapterCallback callBack) {
        callBacks.put(key, callBack);
    }

    @Override
    public void initService() {
        Logger.d(TAG,"MsgPush initSetting start.");
        // 获取消息推送服务
        msgPushService = (MsgPushService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MsgPushSingleServiceID);
        msgPushService.addObserver(this);
        MsgPushInitParam msgPushParam = new MsgPushInitParam();
        msgPushParam.dataPath = GBLCacheFilePath.MSG_FROM_PHONE_PATH; // 消息存储数据库路径，设置目录要有文件创建、读写权限
        msgPushService.init(msgPushParam);
        mBLAosService  = new BLAosService();
        Logger.d(TAG,"MsgPush initSetting success.");
    }

    /**
     * 开启消息推送监听
     * @param userId 消息盒子开启入参，目前只有UserID
     */
    @Override
    public void startListen(String userId) {
        UserLoginInfo userLoginInfo= new UserLoginInfo();
        userLoginInfo.userId = userId;
        if (msgPushService != null) {
            msgPushService.startListen(userLoginInfo);
            Logger.d(TAG,"MsgPush listening started.");
        } else {
            Logger.d(TAG,"Start Listen failed.");
        }
    }

    /**
     * 终止消息推送监听
     */
    @Override
    public void stopListen() {
        if (msgPushService != null) {
            msgPushService.stopListen();
            Logger.d(TAG,"MsgPush listening stopped.");
        } else {
            Logger.d(TAG,"Stop Listen failed.");
        }
    }

    /**
     * 获取推送消息继承的基类的所有数据
     */
    public MsgPushInfo getMsgPushInfo(MsgPushItem msgPushItem) {

        MsgPushInfo msgPushInfo = new MsgPushInfo();
        msgPushInfo.setMessageId(msgPushItem.messageId);
        msgPushInfo.setMessageType(msgPushItem.messageType);
        msgPushInfo.setStatus(msgPushItem.status);
        msgPushInfo.setId(msgPushItem.id);
        msgPushInfo.setBizType(msgPushItem.bizType);
        msgPushInfo.setClientId(msgPushItem.clientId);
        msgPushInfo.setSourceId(msgPushItem.sourceId);
        msgPushInfo.setUserId(msgPushItem.userId);
        msgPushInfo.setCreateTime(msgPushItem.createTime);
        msgPushInfo.setExpiration(msgPushItem.expiration);
        msgPushInfo.setSendTime(msgPushItem.sendTime);
        msgPushInfo.setText(msgPushItem.text);
        msgPushInfo.setTitle(msgPushItem.title);
        msgPushInfo.setVersion(msgPushItem.version);
        msgPushInfo.setAccessKey(msgPushItem.accessKey);
        msgPushInfo.setDeviceId(msgPushItem.deviceId);
        msgPushInfo.setSessionId(msgPushItem.sessionId);
        msgPushInfo.setReaded(msgPushItem.isReaded);
        msgPushInfo.setSendType(msgPushItem.sendType);
        msgPushInfo.setTraceId(msgPushItem.traceId);
        msgPushInfo.setLinkMode(msgPushItem.linkMode);

        return msgPushInfo;
    }


    /**
     * 获取本地历史运营推送消息
     * @return 本地历史运营推送消息数据列表
     */
    @Override
    public ArrayList<MsgPushInfo> getAutoPushMessages() {
        ArrayList<AutoPushMsg> autoPushMsgList = new ArrayList<>();
        ArrayList<MsgPushInfo> autoPushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            autoPushMsgList = msgPushService.getAutoPushMessages();
        }
        for (AutoPushMsg autoPushMsg : autoPushMsgList) {
            MsgPushInfo autoPushMsgInfo = getMsgPushInfo(autoPushMsg);

            AutoPushInfo autoPushInfo = autoPushMsg.content;
            autoPushMsgInfo = GsonUtils.convertToT(autoPushInfo, MsgPushInfo.class);

            autoPushMsgInfoList.add(autoPushMsgInfo);
        }

        if (!autoPushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"autoPushMsgInfoList = " + autoPushMsgInfoList.toString());
            return autoPushMsgInfoList;
        } else {
            Logger.d(TAG,"Get AutoPushMsgInfoList failed.");
            return new ArrayList<>();
        }
    }

    /**
     * 获取本地历史AIMPOI(send2car)推送消息
     * @return 本地历史AIMPOI(send2car)推送消息数据列表
     */
    @Override
    public ArrayList<MsgPushInfo> getAimPoiPushMessages() {

        ArrayList<AimPoiPushMsg> mAimPoiPushMsgList = new ArrayList<>();
        ArrayList<MsgPushInfo> mAimPoiPushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            mAimPoiPushMsgList = msgPushService.getAimPoiPushMessages();
        }

        for (AimPoiPushMsg mAimPoiPushMsg : mAimPoiPushMsgList) {

            MsgPushInfo mAimPoiPushMsgInfo = getMsgPushInfo(mAimPoiPushMsg);

            AimPoiInfo aimPoiInfo = mAimPoiPushMsg.content;

            mAimPoiPushMsgInfo = GsonUtils.convertToT(aimPoiInfo, MsgPushInfo.class);

            mAimPoiPushMsgInfoList.add(mAimPoiPushMsgInfo);
        }

        if (!mAimPoiPushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"mAimPoiPushMsgInfoList = " + mAimPoiPushMsgInfoList.toString());
            return mAimPoiPushMsgInfoList;
        } else {
            Logger.d(TAG,"Get AimPoiPushMsgInfoList failed.");
            return new ArrayList<>();
        }
    }

    private MobileRouteParamInfo getMobileRouteParamBaseInfo(MobileRouteParam routeParam) {

        MobileRouteParamInfo routeParamInfo;

        routeParamInfo = GsonUtils.convertToT(routeParam, MobileRouteParamInfo.class);
        routeParamInfo.setDestinationType(routeParam.destination.type);
        MobileDestination destination = routeParam.destination;
        routeParamInfo = GsonUtils.convertToT(destination, MobileRouteParamInfo.class);

        return routeParamInfo;
    }

    private ArrayList<RoutePathProjectPoints> getRoutePathPoints(AimRoutePushMsg mAimRoutePushMsg, String type) {
        ArrayList<RoutePathProjectPoints> routePathPointsList = new ArrayList<>();
        ArrayList<RoutepathrestorationPointInfo>  routePathPointsInfoList = new ArrayList<>();
        if (Objects.equals(type, "ViaPoints")) {
            routePathPointsInfoList = mAimRoutePushMsg.content.routeParam.viaPoints;
        } else if (Objects.equals(type, "EndPoints")) {
            routePathPointsInfoList = mAimRoutePushMsg.content.routeParam.endPoints;
        } else if (Objects.equals(type, "StartPoints")) {
            routePathPointsInfoList = mAimRoutePushMsg.content.routeParam.startPoints;
        }
        for (RoutepathrestorationPointInfo viaPoint :routePathPointsInfoList) {
            RoutePathProjectPoints viaPointInfo = new RoutePathProjectPoints();
            GsonUtils.copyBean(viaPoint, viaPointInfo);
            routePathPointsList.add(viaPointInfo);
        }
        return routePathPointsList;
    }

    private MsgPushInfo getAimRoutePushData(AimRoutePushMsg mAimRoutePushMsg) {
        MsgPushInfo mAimRoutePushMsgInfo = getMsgPushInfo(mAimRoutePushMsg);

        AimRoutePushInfo aimRoutePushInfo = mAimRoutePushMsg.content;
        mAimRoutePushMsgInfo = GsonUtils.convertToT(aimRoutePushInfo, MsgPushInfo.class);

        mAimRoutePushMsgInfo.setRouteParam(getMobileRouteParamBaseInfo(mAimRoutePushMsg.content.routeParam));

        MobileVehicleInfo vehicle = new MobileVehicleInfo();
        GsonUtils.copyBean(mAimRoutePushMsg.content.routeParam.vehicle, vehicle);
        mAimRoutePushMsgInfo.getRouteParam().setVehicle(vehicle);

        MobileLocation mLocation = new MobileLocation();
        GsonUtils.copyBean(mAimRoutePushMsg.content.routeParam.location, mLocation);
        mAimRoutePushMsgInfo.getRouteParam().setLocation(mLocation);


        ArrayList<MobileRouteViaPointInfo> aimRouteViaPoints = new ArrayList<>();
        for (MobileRouteViaPoint viaPoint : mAimRoutePushMsg.content.routeParam.routeViaPoints) {
            MobileRouteViaPointInfo mobileRouteViaPointInfo = new MobileRouteViaPointInfo();
            GsonUtils.copyBean(viaPoint, mobileRouteViaPointInfo);
            aimRouteViaPoints.add(mobileRouteViaPointInfo);
        }
        mAimRoutePushMsgInfo.routeParam.routeViaPoints = aimRouteViaPoints;

        mAimRoutePushMsgInfo.routeParam.startPoints = getRoutePathPoints(mAimRoutePushMsg, "StartPoints");

        mAimRoutePushMsgInfo.routeParam.viaPoints = getRoutePathPoints(mAimRoutePushMsg, "ViaPoints");

        mAimRoutePushMsgInfo.routeParam.endPoints = getRoutePathPoints(mAimRoutePushMsg, "EndPoints");

        return mAimRoutePushMsgInfo;
    }


    /**
     * 获取本地历史中手机发送的路线推送消息
     * @return 本地历史中手机发送的推送消息数据列表
     */
    @Override
    public ArrayList<MsgPushInfo> getAimRoutePushMessages() {

        ArrayList<AimRoutePushMsg> mAimRoutePushMsgList = new ArrayList<>();
        ArrayList<MsgPushInfo> mAimRoutePushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            mAimRoutePushMsgList = msgPushService.getAimRoutePushMessages();
        }
        Logger.d(TAG,"mAimRoutePushMsgList = " + mAimRoutePushMsgList);
        for (AimRoutePushMsg mAimRoutePushMsg : mAimRoutePushMsgList) {
            mAimRoutePushMsgInfoList.add(getAimRoutePushData(mAimRoutePushMsg));
        }

        if (!mAimRoutePushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"mAimRoutePushMsgInfoList = " + mAimRoutePushMsgInfoList);

            return mAimRoutePushMsgInfoList;
        } else {
            Logger.d(TAG,"Get AimRoutePushMsgInfoList failed.");
            return new ArrayList<>();
        }
    }

    /**
     * 修改手机推送的路线终点POI的名称
     * @param msgId 推送消息id
     * @param msgName 路线终点名称
     */
    @Override
    public void updateAimRouteEndPoiName(long msgId, String msgName) {
        if (msgPushService != null) {
            msgPushService.updateAimRouteEndPoiName(msgId, msgName);
            Logger.d(TAG,"AimRoute end POI name updated.");
        } else {
            Logger.d(TAG,"MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,send2phone服务接口
     * @param pAosRequest 网络请求,内存由HMI管理,ts,车机端send2phone服务接口
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @Override
    public long sendReqSendToPhone(MsgPushRequestInfo pAosRequest) {
        GSendToPhoneRequestParam gSendToPhoneRequestParam = new GSendToPhoneRequestParam();
        GsonUtils.copyBean(pAosRequest, gSendToPhoneRequestParam);
        GAimpoiMsg aimpoiMsg;
        aimpoiMsg = GsonUtils.convertToT(pAosRequest, GAimpoiMsg.class);
        gSendToPhoneRequestParam.aimpoiMsg = aimpoiMsg;

        // TODO
        if (mBLAosService != null) {
            Logger.d(TAG,"SendToPhone request sent.");
            return mBLAosService.sendReqSendToPhone(gSendToPhoneRequestParam, this);
        } else {
            Logger.d(TAG,"MsgPush service is not initialized.");
            return -1;
        }
    }

    /**
     * 连接设备请求
     * @param deviceId 连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public MsgPushResponseInfo request(long deviceId, GeoPoint userLocation) {
        MobileLinkRequest request = new MobileLinkRequest();
        request.deviceId = deviceId;
        MsgPushResponseInfo result = new MsgPushResponseInfo();
        TaskResult result1;
        if (msgPushService != null) {
            result1 = msgPushService.request(request, this);
            GsonUtils.copyBean(result1, result);
            return  result;
        }else {
            Logger.d(TAG,"MsgPush service is not initialized.");
            return new MsgPushResponseInfo();
        }
    }

    /**
     * 终止所有请求
     */
    @Override
    public void abort() {
        if (msgPushService != null) {
            msgPushService.abort();
            Logger.d(TAG,"All requests aborted.");
        } else {
            Logger.d(TAG,"MsgPush service is not initialized.");
        }
    }

    /**
     * 终止指定任务额请求
     * @param taskId task id
     */
    @Override
    public void abort(long taskId) {
        if (msgPushService != null) {
            msgPushService.abort(taskId);
            Logger.d(TAG,"Request with taskId " + taskId + " aborted.");
        } else {
            Logger.d(TAG,"MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param pAosRequest 	网络请求, 内存由HMI管理 ,车机互联的通用上报接口(除去导航上报)
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @Override
    public long sendReqWsTserviceInternalLinkAutoReport(MsgPushRequestInfo pAosRequest) {

        GWsTserviceInternalLinkAutoReportRequestParam gWsTserviceInternalLinkAutoReportRequestParam = new GWsTserviceInternalLinkAutoReportRequestParam();
        gWsTserviceInternalLinkAutoReportRequestParam = GsonUtils.convertToT(pAosRequest, GWsTserviceInternalLinkAutoReportRequestParam.class);

        if (mBLAosService != null) {
            return mBLAosService.sendReqWsTserviceInternalLinkAutoReport(gWsTserviceInternalLinkAutoReportRequestParam, this);
        } else {
            Logger.d(TAG,"MsgPush service is not initialized.");
        }
        return -1;
    }

    /**
     * 运营推送消息通知
     * @param msg 运营推送消息
     */
    // @Override
    public void notifyMessage(AutoPushMsg msg) {

        MsgPushInfo autoPushMsgInfo = getMsgPushInfo(msg);

        AutoPushInfo autoPushInfo = msg.content;

        autoPushMsgInfo = GsonUtils.convertToT(autoPushInfo, MsgPushInfo.class);

        Logger.d(TAG,"AutoPushMsgInfo: " + autoPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.notifyAutoPushMessage(autoPushMsgInfo);
        }
    }

    /**
     * AIMPOI(send2car)推送消息通知
     * @param msg AIMPOI(send2car)推送消息
     */
    @Override
    public void notifyMessage(AimPoiPushMsg msg) {
        MsgPushInfo aimPoiPushMsgInfo = getMsgPushInfo(msg);

        AimPoiInfo aimPoiInfo = msg.content;
        aimPoiPushMsgInfo = GsonUtils.convertToT(aimPoiInfo, MsgPushInfo.class);

        Logger.d(TAG,"AimPoiPushMsgInfo: " + aimPoiPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.notifyAimPoiPushMessage(aimPoiPushMsgInfo);
        }
    }

    /**
     * 手机发送路线推送消息通知
     * @param msg 手机发送路线推送消息
     */
    @Override
    public void notifyMessage(AimRoutePushMsg msg) {
        Logger.d(TAG,"notifyMessage: " + GsonUtils.toJson(msg.content));

        RouteMsgPushInfo routeMsgPushInfo = new RouteMsgPushInfo();
        routeMsgPushInfo.setMsgPushInfo(msg);
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setName(msg.content.routeParam.destination.name);
        poiInfoEntity.setPid(msg.content.routeParam.destination.poiId);
        GeoPoint geoPoint = new GeoPoint();
        geoPoint.lat = Double.parseDouble(msg.content.routeParam.endPoints.get(0).lat);
        geoPoint.lon = Double.parseDouble(msg.content.routeParam.endPoints.get(0).lon);
        poiInfoEntity.setPoint(geoPoint);
        routeMsgPushInfo.setPoiInfoEntity(poiInfoEntity);

        RoutePoint startPoint = new RoutePoint();
        startPoint.setIsDraw(true);
        startPoint.setType(0);
        startPoint.setPathId(0);
        GeoPoint startGeoPoint = new GeoPoint();
        startGeoPoint.lon = Double.parseDouble(msg.content.path.startPoints.points.get(0).lon);
        startGeoPoint.lat = Double.parseDouble(msg.content.path.startPoints.points.get(0).lat);
        startGeoPoint.z = 0;
        startPoint.setPos(startGeoPoint);
        routeMsgPushInfo.setStartPoint(startPoint);

        RoutePoint endPoint = new RoutePoint();
        endPoint.setIsDraw(true);
        endPoint.setType(1);
        endPoint.setPathId(0);
        GeoPoint endGeoPoint = new GeoPoint();
        endGeoPoint.lon = Double.parseDouble(msg.content.path.endPoints.points.get(0).lon);
        endGeoPoint.lat = Double.parseDouble(msg.content.path.endPoints.points.get(0).lat);
        endGeoPoint.z = 0;
        endPoint.setPos(endGeoPoint);
        routeMsgPushInfo.setEndPoint(endPoint);

        ArrayList<RoutePoint> viaPoints = new ArrayList<>();
        ArrayList<PoiInfoEntity> viaPoiInfoEntitys = new ArrayList<>();
        if (msg.content.path.routeViaPoints.display_points != null
                && !msg.content.path.routeViaPoints.display_points.isEmpty()) {
            for (RouteDisplayPoints displayPoint : msg.content.path.routeViaPoints.display_points) {
                RoutePoint viaPoint = new RoutePoint();
                viaPoint.setIsDraw(true);
                viaPoint.setType(2);
                viaPoint.setPathId(0);
                GeoPoint viaGeoPoint = new GeoPoint();
                viaGeoPoint.lon = Double.parseDouble(displayPoint.lon);
                viaGeoPoint.lat = Double.parseDouble(displayPoint.lat);
                viaGeoPoint.z = 0;
                viaPoint.setPos(viaGeoPoint);
                viaPoints.add(viaPoint);

                PoiInfoEntity viaPoiInfoEntity = new PoiInfoEntity();
                viaPoiInfoEntity.setName(displayPoint.poiName);
                viaPoiInfoEntity.setPid(displayPoint.poiID);
                viaPoiInfoEntity.setPoint(viaGeoPoint);
                viaPoiInfoEntitys.add(viaPoiInfoEntity);
            }
        }
        routeMsgPushInfo.setViaPoints(viaPoints);
        routeMsgPushInfo.setViaPoiInfoEntity(viaPoiInfoEntitys);

        if (msg.content.routeParam.destination != null) {
            routeMsgPushInfo.setName(msg.content.routeParam.destination.name);
        }

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.notifyAimRoutePushMessage(routeMsgPushInfo);
        }
    }

    /**
     * 手机端推送发现可连接车机消息
     * @param msg 消息数据
     */
    @Override
    public void notifyMessage(MobileLinkPushMsg msg) {

        MsgPushInfo mobileLinkPushMsgInfo = getMsgPushInfo(msg);

        Logger.d(TAG,"MobileLinkPushMsgInfo: " + mobileLinkPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.notifyMobileLinkPushMessage(mobileLinkPushMsgInfo);
        }
    }

    private MsgPushResponseInfo getResponseBase(BLResponseBase info) {
        MsgPushResponseInfo msgPushResponseInfo;

        msgPushResponseInfo = GsonUtils.convertToT(info, MsgPushResponseInfo.class);

        ArrayList<PropertyValueInfo> blKeyValues = new ArrayList<>();

        for (BLKeyValue blKeyValue : info.headers.property) {
            PropertyValueInfo blKeyValueInfo = new PropertyValueInfo();
            blKeyValueInfo.setmStrKey(blKeyValue.m_strKey);
            blKeyValueInfo.setmStrValue(blKeyValue.m_strValue);
            blKeyValues.add(blKeyValueInfo);
        }
        msgPushResponseInfo.property = blKeyValues;

        return msgPushResponseInfo;
    }

    /**
     * 网络库线程中回调业务应答类
     * @param gSendToPhoneResponseParam send2phone服务接口
     */
    @Override
    public void onRecvAck(GSendToPhoneResponseParam gSendToPhoneResponseParam) {
        MsgPushResponseInfo msgPushResponseInfo = getResponseBase(gSendToPhoneResponseParam);
        msgPushResponseInfo.setmEAosRequestType(1100001);

        Logger.d(TAG,"GSendToPhoneResponseParamInfo: " + msgPushResponseInfo.toString());

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.onRecvAckGSendToPhoneResponse(getResponseBase(gSendToPhoneResponseParam));
        }
    }

    /**
     * 网络库线程中回调业务应答类
     * @param gWsTserviceInternalLinkAutoReportResponseParam 车机互联的通用上报接口(除去导航上报)，应答类
     */
    @Override
    public void onRecvAck(GWsTserviceInternalLinkAutoReportResponseParam gWsTserviceInternalLinkAutoReportResponseParam) {

        MsgPushResponseInfo msgPushResponseInfo = getResponseBase(gWsTserviceInternalLinkAutoReportResponseParam);
        msgPushResponseInfo.setmEAosRequestType(1600041);
        msgPushResponseInfo.setmNetworkStatus(0);

        Logger.d(TAG,"GWsTserviceInternalLinkAutoReportResponseParamInfo: " + msgPushResponseInfo.toString());

        for (MsgPushAdapterCallback callBack : callBacks.values()) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(msgPushResponseInfo);
        }
    }
}