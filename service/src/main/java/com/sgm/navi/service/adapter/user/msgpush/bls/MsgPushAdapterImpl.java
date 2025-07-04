package com.sgm.navi.service.adapter.user.msgpush.bls;

import com.android.utils.ConvertUtils;
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
import com.autonavi.gbl.user.msgpush.model.LinkStatusPushMsg;
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
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.msgpush.IMsgPushApi;
import com.sgm.navi.service.adapter.user.msgpush.MsgPushAdapterCallback;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RoutePoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.msgpush.MobileLocation;
import com.sgm.navi.service.define.user.msgpush.MobileRouteParamInfo;
import com.sgm.navi.service.define.user.msgpush.MobileRouteViaPointInfo;
import com.sgm.navi.service.define.user.msgpush.MobileVehicleInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushResponseInfo;
import com.sgm.navi.service.define.user.msgpush.PropertyValueInfo;
import com.sgm.navi.service.define.user.msgpush.RoutePathProjectPoints;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Objects;

public class MsgPushAdapterImpl implements IMsgPushApi, IMsgPushServiceObserver, ICallBackSendToPhone,
    ICallBackWsTserviceInternalLinkAutoReport, IMobileLinkObserver{

    private static final String TAG = MapDefaultFinalTag.MSG_PUSH_SERVICE_TAG;

    private MsgPushService msgPushService;
    private BLAosService mBLAosService;
    private final Hashtable<String, MsgPushAdapterCallback> mCallBacks;

    public MsgPushAdapterImpl() {
        mCallBacks = new Hashtable<>();
    }
    @Override
    public void registerCallBack(final String key, final MsgPushAdapterCallback callBack) {
        mCallBacks.put(key, callBack);
    }

    @Override
    public void initService() {
        Logger.d(TAG,"MsgPush initSetting start.");
        // 获取消息推送服务
        msgPushService = (MsgPushService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MsgPushSingleServiceID);
        msgPushService.addObserver(this);
        final MsgPushInitParam msgPushParam = new MsgPushInitParam();
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
    public void startListen(final String userId) {
        final UserLoginInfo userLoginInfo= new UserLoginInfo();
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
     * @param msgPushItem
     * @return MsgPushInfo
     */
    public MsgPushInfo getMsgPushInfo(final MsgPushItem msgPushItem) {

        final MsgPushInfo msgPushInfo = new MsgPushInfo();
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
        final ArrayList<MsgPushInfo> autoPushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            autoPushMsgList = msgPushService.getAutoPushMessages();
        }
        for (AutoPushMsg autoPushMsg : autoPushMsgList) {
            MsgPushInfo autoPushMsgInfo = getMsgPushInfo(autoPushMsg);

            final AutoPushInfo autoPushInfo = autoPushMsg.content;
            autoPushMsgInfo = GsonUtils.convertToT(autoPushInfo, MsgPushInfo.class);

            autoPushMsgInfoList.add(autoPushMsgInfo);
        }

        if (!autoPushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"autoPushMsgInfoList = " , autoPushMsgInfoList.toString());
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

        ArrayList<AimPoiPushMsg> aimPoiPushMsgList = new ArrayList<>();
        final ArrayList<MsgPushInfo> aimPoiPushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            aimPoiPushMsgList = msgPushService.getAimPoiPushMessages();
        }

        for (AimPoiPushMsg aimPoiPushMsg : aimPoiPushMsgList) {

            MsgPushInfo aimPoiPushMsgInfo = getMsgPushInfo(aimPoiPushMsg);

            final AimPoiInfo aimPoiInfo = aimPoiPushMsg.content;

            aimPoiPushMsgInfo = GsonUtils.convertToT(aimPoiInfo, MsgPushInfo.class);

            aimPoiPushMsgInfoList.add(aimPoiPushMsgInfo);
        }

        if (!aimPoiPushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"mAimPoiPushMsgInfoList = " , aimPoiPushMsgInfoList.toString());
            return aimPoiPushMsgInfoList;
        } else {
            Logger.d(TAG,"Get AimPoiPushMsgInfoList failed.");
            return new ArrayList<>();
        }
    }

    /**
     * getMobileRouteParamBaseInfo
     * @param routeParam
     * @return MobileRouteParamInfo
     */
    private MobileRouteParamInfo getMobileRouteParamBaseInfo(final MobileRouteParam routeParam) {

        MobileRouteParamInfo routeParamInfo;

        routeParamInfo = GsonUtils.convertToT(routeParam, MobileRouteParamInfo.class);
        routeParamInfo.setDestinationType(routeParam.destination.type);
        final MobileDestination destination = routeParam.destination;
        routeParamInfo = GsonUtils.convertToT(destination, MobileRouteParamInfo.class);

        return routeParamInfo;
    }

    /**
     * getRoutePathPoints
     * @param aimRoutePushMsg
     * @param type
     * @return list
     */
    private ArrayList<RoutePathProjectPoints> getRoutePathPoints(final AimRoutePushMsg aimRoutePushMsg, final String type) {
        final ArrayList<RoutePathProjectPoints> routePathPointsList = new ArrayList<>();
        ArrayList<RoutepathrestorationPointInfo>  routePathPointsInfoList = new ArrayList<>();
        if (Objects.equals(type, "ViaPoints")) {
            routePathPointsInfoList = aimRoutePushMsg.content.routeParam.viaPoints;
        } else if (Objects.equals(type, "EndPoints")) {
            routePathPointsInfoList = aimRoutePushMsg.content.routeParam.endPoints;
        } else if (Objects.equals(type, "StartPoints")) {
            routePathPointsInfoList = aimRoutePushMsg.content.routeParam.startPoints;
        }
        for (RoutepathrestorationPointInfo viaPoint :routePathPointsInfoList) {
            final RoutePathProjectPoints viaPointInfo = new RoutePathProjectPoints();
            GsonUtils.copyBean(viaPoint, viaPointInfo);
            routePathPointsList.add(viaPointInfo);
        }
        return routePathPointsList;
    }

    /**
     * getAimRoutePushData
     * @param aimRoutePushMsg
     * @return MsgPushInfo
     */
    private MsgPushInfo getAimRoutePushData(final AimRoutePushMsg aimRoutePushMsg) {
        MsgPushInfo aimRoutePushMsgInfo = getMsgPushInfo(aimRoutePushMsg);

        final AimRoutePushInfo aimRoutePushInfo = aimRoutePushMsg.content;
        aimRoutePushMsgInfo = GsonUtils.convertToT(aimRoutePushInfo, MsgPushInfo.class);

        aimRoutePushMsgInfo.setRouteParam(getMobileRouteParamBaseInfo(aimRoutePushMsg.content.routeParam));

        final MobileVehicleInfo vehicle = new MobileVehicleInfo();
        GsonUtils.copyBean(aimRoutePushMsg.content.routeParam.vehicle, vehicle);
        aimRoutePushMsgInfo.getRouteParam().setVehicle(vehicle);

        final MobileLocation location = new MobileLocation();
        GsonUtils.copyBean(aimRoutePushMsg.content.routeParam.location, location);
        aimRoutePushMsgInfo.getRouteParam().setLocation(location);


        final ArrayList<MobileRouteViaPointInfo> aimRouteViaPoints = new ArrayList<>();
        for (MobileRouteViaPoint viaPoint : aimRoutePushMsg.content.routeParam.routeViaPoints) {
            final MobileRouteViaPointInfo mobileRouteViaPointInfo = new MobileRouteViaPointInfo();
            GsonUtils.copyBean(viaPoint, mobileRouteViaPointInfo);
            aimRouteViaPoints.add(mobileRouteViaPointInfo);
        }
        aimRoutePushMsgInfo.routeParam.routeViaPoints = aimRouteViaPoints;

        aimRoutePushMsgInfo.routeParam.startPoints = getRoutePathPoints(aimRoutePushMsg, "StartPoints");

        aimRoutePushMsgInfo.routeParam.viaPoints = getRoutePathPoints(aimRoutePushMsg, "ViaPoints");

        aimRoutePushMsgInfo.routeParam.endPoints = getRoutePathPoints(aimRoutePushMsg, "EndPoints");

        return aimRoutePushMsgInfo;
    }


    /**
     * 获取本地历史中手机发送的路线推送消息
     * @return 本地历史中手机发送的推送消息数据列表
     */
    @Override
    public ArrayList<MsgPushInfo> getAimRoutePushMessages() {

        ArrayList<AimRoutePushMsg> aimRoutePushMsgList = new ArrayList<>();
        final ArrayList<MsgPushInfo> aimRoutePushMsgInfoList = new ArrayList<>();
        if (msgPushService != null) {
            aimRoutePushMsgList = msgPushService.getAimRoutePushMessages();
        }
        Logger.d(TAG,"mAimRoutePushMsgList = " + aimRoutePushMsgList);
        for (AimRoutePushMsg aimRoutePushMsg : aimRoutePushMsgList) {
            aimRoutePushMsgInfoList.add(getAimRoutePushData(aimRoutePushMsg));
        }

        if (!aimRoutePushMsgInfoList.isEmpty()) {

            Logger.d(TAG,"mAimRoutePushMsgInfoList = " + aimRoutePushMsgInfoList);

            return aimRoutePushMsgInfoList;
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
    public void updateAimRouteEndPoiName(final long msgId, final String msgName) {
        if (msgPushService != null) {
            msgPushService.updateAimRouteEndPoiName(msgId, msgName);
            Logger.d(TAG,"AimRoute end POI name updated.");
        } else {
            Logger.d(TAG,"updateAimRouteEndPoiName: MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,send2phone服务接口
     * @param aosRequest 网络请求,内存由HMI管理,ts,车机端send2phone服务接口
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_PHONE_DESTINATION_TO)
    public long sendReqSendToPhone(final MsgPushRequestInfo aosRequest) {
        final GSendToPhoneRequestParam sendToPhoneRequestParam = new GSendToPhoneRequestParam();
        GsonUtils.copyBean(aosRequest, sendToPhoneRequestParam);
        sendToPhoneRequestParam.aimpoiMsg = GsonUtils.convertToT(aosRequest, GAimpoiMsg.class);

        //For Bury Point
        final BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, aosRequest.getName())
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);

        // TODO
        if (mBLAosService != null) {
            Logger.d(TAG,"SendToPhone request sent.");
            return mBLAosService.sendReqSendToPhone(sendToPhoneRequestParam, this);
        } else {
            Logger.d(TAG,"sendReqSendToPhone:MsgPush service is not initialized.");
            return -1;
        }
    }

    /**
     * 连接设备请求
     * @param deviceId 连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public MsgPushResponseInfo request(final long deviceId, final GeoPoint userLocation) {
        final MobileLinkRequest request = new MobileLinkRequest();
        request.deviceId = deviceId;
        final MsgPushResponseInfo result = new MsgPushResponseInfo();
        final TaskResult result1;
        if (msgPushService != null) {
            result1 = msgPushService.request(request, this);
            GsonUtils.copyBean(result1, result);
            return  result;
        }else {
            Logger.d(TAG,"request:MsgPush service is not initialized.");
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
            Logger.d(TAG,"abort:MsgPush service is not initialized.");
        }
    }

    /**
     * 终止指定任务额请求
     * @param taskId task id
     */
    @Override
    public void abort(final long taskId) {
        if (msgPushService != null) {
            msgPushService.abort(taskId);
            Logger.d(TAG,"Request with taskId " + taskId + " aborted.");
        } else {
            Logger.d(TAG,"abort task id: MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param aosRequest 网络请求, 内存由HMI管理 ,车机互联的通用上报接口(除去导航上报)
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @Override
    public long sendReqWsTserviceInternalLinkAutoReport(final MsgPushRequestInfo aosRequest) {

        final GWsTserviceInternalLinkAutoReportRequestParam gwsTserviceInternalLinkAutoReportRequestParam =
            GsonUtils.convertToT(aosRequest, GWsTserviceInternalLinkAutoReportRequestParam.class);

        if (mBLAosService != null) {
            return mBLAosService.sendReqWsTserviceInternalLinkAutoReport(gwsTserviceInternalLinkAutoReportRequestParam, this);
        } else {
            Logger.d(TAG,"sendReqWsTserviceInternalLinkAutoReport:MsgPush service is not initialized.");
        }
        return -1;
    }

    /**
     * 运营推送消息通知
     * @param msg 运营推送消息
     */
    // @Override
    public void notifyMessage(final AutoPushMsg msg) {

        MsgPushInfo autoPushMsgInfo = getMsgPushInfo(msg);

        final AutoPushInfo autoPushInfo = msg.content;

        autoPushMsgInfo = GsonUtils.convertToT(autoPushInfo, MsgPushInfo.class);

        Logger.d(TAG,"AutoPushMsgInfo: " + autoPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyAutoPushMessage(autoPushMsgInfo);
        }
    }

    /**
     * AIMPOI(send2car)推送消息通知
     * @param msg AIMPOI(send2car)推送消息
     */
    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_PHONE_DESTINATION_FROM)
    public void notifyMessage(final AimPoiPushMsg msg) {
        MsgPushInfo aimPoiPushMsgInfo = getMsgPushInfo(msg);

        final AimPoiInfo aimPoiInfo = msg.content;
        aimPoiPushMsgInfo = GsonUtils.convertToT(aimPoiInfo, MsgPushInfo.class);

        Logger.d(TAG,"AimPoiPushMsgInfo: " + aimPoiPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyAimPoiPushMessage(aimPoiPushMsgInfo);
        }
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS,
                        aimPoiPushMsgInfo.getName() != null ? aimPoiPushMsgInfo.getName() : "")
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 手机发送路线推送消息通知
     * @param msg 手机发送路线推送消息
     */
    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_PHONE_DESTINATION_FROM)
    public void notifyMessage(final AimRoutePushMsg msg) {
        if (msg == null) {
            Logger.e(TAG,"msg is null");
            return;
        }
        Logger.d(TAG,"notifyMessage: " + GsonUtils.toJson(msg.content));

        final RouteMsgPushInfo routeMsgPushInfo = new RouteMsgPushInfo();
        routeMsgPushInfo.setMMsgPushInfo(msg);
        routeMsgPushInfo.setMSendType(msg.content.sendMode);
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setName(msg.content.routeParam.destination.name);
        poiInfoEntity.setPid(msg.content.routeParam.destination.poiId);
        final ArrayList<RoutepathrestorationPointInfo> endPoints = msg.content.routeParam.endPoints;
        final GeoPoint geoPoint = new GeoPoint();
        if (!ConvertUtils.isEmpty(endPoints) && endPoints.get(0) != null ) {
            geoPoint.setLat(Double.parseDouble(endPoints.get(0).lat));
            geoPoint.setLon(Double.parseDouble(endPoints.get(0).lon));
        }
        poiInfoEntity.setPoint(geoPoint);
        routeMsgPushInfo.setMPoiInfoEntity(poiInfoEntity);

        final RoutePoint startPoint = new RoutePoint();
        startPoint.setMIsDraw(true);
        startPoint.setMType(0);
        startPoint.setMPathId(0);
        final GeoPoint startGeoPoint = new GeoPoint();
        if (!ConvertUtils.isEmpty(endPoints) && endPoints.get(0) != null) {
            startGeoPoint.setLon(Double.parseDouble(endPoints.get(0).lon));
            startGeoPoint.setLat(Double.parseDouble(endPoints.get(0).lat));
        }
        startGeoPoint.setZ( 0);
        startPoint.setMPos(startGeoPoint);
        routeMsgPushInfo.setMStartPoint(startPoint);

        final RoutePoint endPoint = new RoutePoint();
        endPoint.setMIsDraw(true);
        endPoint.setMType(1);
        endPoint.setMPathId(0);
        final GeoPoint endGeoPoint = new GeoPoint();
        if (!ConvertUtils.isEmpty(endPoints) && endPoints.get(0) != null) {
            endGeoPoint.setLon(Double.parseDouble(endPoints.get(0).lon));
            endGeoPoint.setLat(Double.parseDouble(endPoints.get(0).lat));
        }

        endGeoPoint.setZ(0);
        endPoint.setMPos(endGeoPoint);
        routeMsgPushInfo.setMEndPoint(endPoint);

        final ArrayList<RoutePoint> viaPoints = new ArrayList<>();
        final ArrayList<PoiInfoEntity> viaPoiInfoEntitys = new ArrayList<>();
        if (msg.content.path.routeViaPoints.display_points != null
                && !msg.content.path.routeViaPoints.display_points.isEmpty()) {
            for (RouteDisplayPoints displayPoint : msg.content.path.routeViaPoints.display_points) {
                final RoutePoint viaPoint = new RoutePoint();
                viaPoint.setMIsDraw(true);
                viaPoint.setMType(2);
                viaPoint.setMPathId(0);
                final GeoPoint viaGeoPoint = new GeoPoint();
                viaGeoPoint.setLon(Double.parseDouble(displayPoint.lon));
                viaGeoPoint.setLat(Double.parseDouble(displayPoint.lat));
                viaGeoPoint.setZ(0);
                viaPoint.setMPos(viaGeoPoint);
                viaPoints.add(viaPoint);

                final PoiInfoEntity viaPoiInfoEntity = new PoiInfoEntity();
                viaPoiInfoEntity.setName(displayPoint.poiName);
                viaPoiInfoEntity.setPid(displayPoint.poiID);
                viaPoiInfoEntity.setPoint(viaGeoPoint);
                viaPoiInfoEntitys.add(viaPoiInfoEntity);
            }
        }
        routeMsgPushInfo.setMViaPoints(viaPoints);
        routeMsgPushInfo.setMViaPoiInfoEntity(viaPoiInfoEntitys);

        if (msg.content.routeParam.destination != null) {
            routeMsgPushInfo.setMName(msg.content.routeParam.destination.name);
        }

        final BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, msg.content.routeParam.destination != null ? msg.content.routeParam.destination.name : "").build();
        BuryPointController.getInstance().setBuryProps(buryProperty);

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyAimRoutePushMessage(routeMsgPushInfo);
        }
    }

    /**
     * 手机端推送发现可连接车机消息
     * @param msg 消息数据
     */
    @Override
    public void notifyMessage(final MobileLinkPushMsg msg) {

        final MsgPushInfo mobileLinkPushMsgInfo = getMsgPushInfo(msg);

        Logger.d(TAG,"MobileLinkPushMsgInfo: " + mobileLinkPushMsgInfo.toString());

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyMobileLinkPushMessage(mobileLinkPushMsgInfo);
        }
    }

    /**
     * getResponseBase
     * @param info
     * @return MsgPushResponseInfo
     */
    private MsgPushResponseInfo getResponseBase(final BLResponseBase info) {
        final MsgPushResponseInfo msgPushResponseInfo = GsonUtils.convertToT(info, MsgPushResponseInfo.class);
        final ArrayList<PropertyValueInfo> blKeyValues = new ArrayList<>();

        for (BLKeyValue blKeyValue : info.headers.property) {
            final PropertyValueInfo blKeyValueInfo = new PropertyValueInfo();
            blKeyValueInfo.setStrKey(blKeyValue.m_strKey);
            blKeyValueInfo.setStrValue(blKeyValue.m_strValue);
            blKeyValues.add(blKeyValueInfo);
        }
        msgPushResponseInfo.property = blKeyValues;

        return msgPushResponseInfo;
    }

    /**
     * 网络库线程中回调业务应答类
     * @param sendToPhoneResponseParam send2phone服务接口
     */
    @Override
    public void onRecvAck(final GSendToPhoneResponseParam sendToPhoneResponseParam) {
        final MsgPushResponseInfo msgPushResponseInfo = getResponseBase(sendToPhoneResponseParam);
        msgPushResponseInfo.setmEAosRequestType(1100001);

        Logger.d(TAG,"GSendToPhoneResponseParamInfo: " + msgPushResponseInfo.toString());

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.onRecvAckGSendToPhoneResponse(getResponseBase(sendToPhoneResponseParam));
        }
    }

    /**
     * 网络库线程中回调业务应答类
     * @param gwsTserviceInternalLinkAutoReportResponseParam 车机互联的通用上报接口(除去导航上报)，应答类
     */
    @Override
    public void onRecvAck(final GWsTserviceInternalLinkAutoReportResponseParam gwsTserviceInternalLinkAutoReportResponseParam) {

        final MsgPushResponseInfo msgPushResponseInfo = getResponseBase(gwsTserviceInternalLinkAutoReportResponseParam);
        msgPushResponseInfo.setmEAosRequestType(1600041);
        msgPushResponseInfo.setmNetworkStatus(0);

        Logger.d(TAG,"GWsTserviceInternalLinkAutoReportResponseParamInfo: " + msgPushResponseInfo.toString());

        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(msgPushResponseInfo);
        }
    }

    @Override
    public void notifyMessage(LinkStatusPushMsg msg) {
        int status = msg.status;
        for (MsgPushAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyDisconnectFromMobileMessage(status);
        }
    }
}