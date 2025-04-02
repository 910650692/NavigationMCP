package com.fy.navi.service.adapter.user.carconnect.bls;

import android.annotation.SuppressLint;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.BLKeyValue;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportRequestParam;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportResponseParam;
import com.autonavi.gbl.aosclient.observer.ICallBackWsTserviceInternalLinkAutoReport;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.msgpush.MsgPushService;
import com.autonavi.gbl.user.msgpush.model.MobileLinkPushMsg;
import com.autonavi.gbl.user.msgpush.model.MobileLinkRequest;
import com.autonavi.gbl.user.msgpush.model.MobileLinkResult;
import com.autonavi.gbl.user.msgpush.model.MobileLinkableDevice;
import com.autonavi.gbl.user.msgpush.model.MobileLinkableRequest;
import com.autonavi.gbl.user.msgpush.model.MobileLinkableResult;
import com.autonavi.gbl.user.msgpush.observer.IMobileLinkObserver;
import com.autonavi.gbl.user.msgpush.observer.IMsgPushServiceObserver;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.autonavi.gbl.util.model.TaskResult;
import com.fy.navi.service.adapter.user.carconnect.CarConnectAdapterCallback;
import com.fy.navi.service.adapter.user.carconnect.ICarConnectApi;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.user.carconnect.CarConnectRequestBaseBean;
import com.fy.navi.service.define.user.carconnect.CarConnectResponseBaseBean;
import com.fy.navi.service.define.user.carconnect.MobileLinkableDeviceBean;
import com.fy.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;
import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;

import java.util.ArrayList;
import java.util.Hashtable;


public class CarConnectAdapterImpl implements ICarConnectApi, IMobileLinkObserver, IMsgPushServiceObserver,
        ICallBackWsTserviceInternalLinkAutoReport {

    private MsgPushService mMsgPushService;
    private BLAosService mBLAosService;
    private final Hashtable<String, CarConnectAdapterCallback> mCallBacks;

    public CarConnectAdapterImpl() {
        mCallBacks = new Hashtable<>();
    }
    @Override
    public void registerCallBack(final String key, final CarConnectAdapterCallback callBack) {
        mCallBacks.put(key, callBack);
    }

    @Override
    public void initService() {
        mMsgPushService = (MsgPushService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MsgPushSingleServiceID);
        mBLAosService  = new BLAosService();
    }

    /**
     * 可连接设备请求
     * @param geoPoint 可连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public TaskResultBean requestMobileLinkable(final GeoPoint geoPoint) {

        final MobileLinkableRequest request = new MobileLinkableRequest();
        request.userLocation.lat = geoPoint.getLat();
        request.userLocation.lon = geoPoint.getLon();

        final TaskResultBean result = new TaskResultBean();
        if (mMsgPushService != null) {
            final TaskResult data = mMsgPushService.request(request, this);
            result.setErrorCode(data.errorCode);
            result.setErrorMessage(data.errorMessage);
            result.setTaskId(data.taskId);
        }
        return  result;
    }

    /**
     * 连接设备请求
     * @param deviceId 连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public TaskResultBean requestMobileLink(final long deviceId) {
        final MobileLinkRequest request = new MobileLinkRequest();
        request.deviceId = deviceId;

        final TaskResultBean result = new TaskResultBean();
        if (mMsgPushService != null) {
            final TaskResult data = mMsgPushService.request(request, this);
            result.setErrorCode(data.errorCode);
            result.setErrorMessage(data.errorMessage);
            result.setTaskId(data.taskId);
        }
        return  result;
    }

    /**
     * 终止所有请求
     */
    @Override
    public void abort() {
        if (mMsgPushService != null) {
            mMsgPushService.abort();
            Logger.d("All requests aborted.");
        } else {
            Logger.d("MsgPush service is not initialized.");
        }
    }

    /**
     * 终止指定任务额请求
     * @param taskId task id
     */
    @Override
    public void abort(final long taskId) {
        if (mMsgPushService != null) {
            mMsgPushService.abort(taskId);
            Logger.d("Request with taskId " + taskId + " aborted.");
        } else {
            Logger.d("MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param aosRequest 网络请求, 内存由HMI管理 ,车机互联的通用上报接口(除去导航上报)
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @SuppressLint("WrongConstant")
    @Override
    public long sendReqWsTserviceInternalLinkAutoReport(final CarConnectRequestBaseBean aosRequest) {
        final GWsTserviceInternalLinkAutoReportRequestParam mGWsTserviceInternalLinkAutoReportRequestParam =
                new GWsTserviceInternalLinkAutoReportRequestParam();
        mGWsTserviceInternalLinkAutoReportRequestParam.data = aosRequest.getData();
        mGWsTserviceInternalLinkAutoReportRequestParam.bizType = aosRequest.getBizType();
        mGWsTserviceInternalLinkAutoReportRequestParam.mEAosRequestType = aosRequest.getmEAosRequestType();
        mGWsTserviceInternalLinkAutoReportRequestParam.mEReqProtol = aosRequest.getmEReqProtol();
        mGWsTserviceInternalLinkAutoReportRequestParam.mEReqMethod = aosRequest.getEReqMethod();
        mGWsTserviceInternalLinkAutoReportRequestParam.mTimeOut = aosRequest.getmTimeOut();
        mGWsTserviceInternalLinkAutoReportRequestParam.mGroup = aosRequest.getmGroup();
        if (mBLAosService != null) {
            return mBLAosService.sendReqWsTserviceInternalLinkAutoReport(mGWsTserviceInternalLinkAutoReportRequestParam, this);
        } else {
            Logger.d("BLAos service is not initialized.");
        }
        return -1;
    }

    @Override
    public void onResult(final MobileLinkableResult result) {
        ThreadManager.getInstance().postUi(() -> {
            final MobileLinkableResultBean mobileLinkableResultBean = new MobileLinkableResultBean();

            final ArrayList<MobileLinkableDeviceBean> devices = new ArrayList<>();
            for (MobileLinkableDevice device : result.devices) {
                final MobileLinkableDeviceBean bean = new MobileLinkableDeviceBean();
                bean.setDeviceId(device.deviceId);
                bean.setManufacture(device.manufacture);
                devices.add(bean);
            }
            mobileLinkableResultBean.setDevices(devices);
            mobileLinkableResultBean.setLottieUrl(result.lottieUrl);
            mobileLinkableResultBean.setErrorCode(result.errorCode);
            mobileLinkableResultBean.setErrorMessage(result.errorMessage);
            mobileLinkableResultBean.setTaskId(result.taskId);

            Logger.d("MobileLinkableResult: " + mobileLinkableResultBean.toString());

            for (CarConnectAdapterCallback callBack : mCallBacks.values()) {
                callBack.onMobileLinkableResult(mobileLinkableResultBean);
            }
        });
    }

    @Override
    public void onResult(final MobileLinkResult result) {
        ThreadManager.getInstance().postUi(() -> {

            final TaskResultBean taskResultBean = new TaskResultBean();
            taskResultBean.setErrorCode(result.errorCode);
            taskResultBean.setErrorMessage(result.errorMessage);
            taskResultBean.setTaskId(result.taskId);

            Logger.d("MobileLinkResult: " + taskResultBean.toString());

            for (CarConnectAdapterCallback callBack : mCallBacks.values()) {
                callBack.onMobileLinkResult(taskResultBean);
            }
        });
    }

    /**
     * 手机端推送发现可连接车机消息
     * @param msg 消息数据
     */
    @Override
    public void notifyMessage(final MobileLinkPushMsg msg) {
        final MsgPushItemBean mobileLinkPushMsgInfo = new MsgPushItemBean();
        mobileLinkPushMsgInfo.messageId = msg.messageId;
        mobileLinkPushMsgInfo.messageType = msg.messageType;
        mobileLinkPushMsgInfo.status = msg.status;
        mobileLinkPushMsgInfo.id = msg.id;
        mobileLinkPushMsgInfo.bizType = msg.bizType;
        mobileLinkPushMsgInfo.clientId = msg.clientId;
        mobileLinkPushMsgInfo.sourceId = msg.sourceId;
        mobileLinkPushMsgInfo.userId = msg.userId;
        mobileLinkPushMsgInfo.createTime = msg.createTime;
        mobileLinkPushMsgInfo.expiration = msg.expiration;
        mobileLinkPushMsgInfo.sendTime = msg.sendTime;
        mobileLinkPushMsgInfo.text = msg.text;
        mobileLinkPushMsgInfo.title = msg.title;
        mobileLinkPushMsgInfo.version = msg.version;
        mobileLinkPushMsgInfo.accessKey = msg.accessKey;
        mobileLinkPushMsgInfo.deviceId = msg.deviceId;
        mobileLinkPushMsgInfo.sessionId = msg.sessionId;
        mobileLinkPushMsgInfo.isReaded = msg.isReaded;
        mobileLinkPushMsgInfo.sendType = msg.sendType;
        mobileLinkPushMsgInfo.traceId = msg.traceId;
        mobileLinkPushMsgInfo.linkMode = msg.linkMode;

        Logger.d("MobileLinkPushMsgInfo: " + mobileLinkPushMsgInfo.toString());

        for (CarConnectAdapterCallback callBack : mCallBacks.values()) {
            callBack.notifyMobileLinkPushMessage(mobileLinkPushMsgInfo);
        }
    }

    /**
     * 网络库线程中回调业务应答类
     * @param wsTserviceInternalLinkAutoReportResponseParam 车机互联的通用上报接口(除去导航上报)，应答类
     */
    @Override
    public void onRecvAck(final GWsTserviceInternalLinkAutoReportResponseParam wsTserviceInternalLinkAutoReportResponseParam) {
        final CarConnectResponseBaseBean responseBaseBean = new CarConnectResponseBaseBean();

        responseBaseBean.setCode(wsTserviceInternalLinkAutoReportResponseParam.mEAosRequestType);
        responseBaseBean.setNetErrorCode(wsTserviceInternalLinkAutoReportResponseParam.mNetErrorCode);
        responseBaseBean.setNetworkStatus(wsTserviceInternalLinkAutoReportResponseParam.mNetworkStatus);
        responseBaseBean.setReqHandle(wsTserviceInternalLinkAutoReportResponseParam.mReqHandle);
        responseBaseBean.setHttpAckCode(wsTserviceInternalLinkAutoReportResponseParam.mHttpAckCode);
        responseBaseBean.setCode(wsTserviceInternalLinkAutoReportResponseParam.code);
        responseBaseBean.setTimestamp(wsTserviceInternalLinkAutoReportResponseParam.timestamp);
        responseBaseBean.setMessage(wsTserviceInternalLinkAutoReportResponseParam.message);
        responseBaseBean.setVersion(wsTserviceInternalLinkAutoReportResponseParam.version);
        responseBaseBean.setResult(wsTserviceInternalLinkAutoReportResponseParam.result);

        final ArrayList<PropertyValueInfo> headers = new ArrayList<>();
        for (BLKeyValue value : wsTserviceInternalLinkAutoReportResponseParam.headers.property) {
            final PropertyValueInfo info = new PropertyValueInfo();
            info.setStrKey(value.m_strKey);
            info.setStrValue(value.m_strValue);
            headers.add(info);
        }
        responseBaseBean.setHeaders(headers);

        Logger.d("responseBaseBean: " + responseBaseBean.toString());

        for (CarConnectAdapterCallback callBack : mCallBacks.values()) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(responseBaseBean);
        }
    }
}
