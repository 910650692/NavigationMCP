package com.fy.navi.service.adapter.user.carconnect.bls;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportRequestParam;
import com.autonavi.gbl.aosclient.model.GWsTserviceInternalLinkAutoReportResponseParam;
import com.autonavi.gbl.aosclient.observer.ICallBackWsTserviceInternalLinkAutoReport;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.msgpush.MsgPushService;
import com.autonavi.gbl.user.msgpush.model.MobileLinkPushMsg;
import com.autonavi.gbl.user.msgpush.model.MobileLinkRequest;
import com.autonavi.gbl.user.msgpush.model.MobileLinkResult;
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
import com.fy.navi.service.define.user.carconnect.MobileLinkableResultBean;
import com.fy.navi.service.define.user.carconnect.TaskResultBean;
import com.fy.navi.service.define.user.group.MsgPushItemBean;

import java.util.Hashtable;


public class CarConnectAdapterImpl implements ICarConnectApi, IMobileLinkObserver, IMsgPushServiceObserver, ICallBackWsTserviceInternalLinkAutoReport {

    private MsgPushService msgPushService;
    private BLAosService mBLAosService;
    private final Hashtable<String, CarConnectAdapterCallback> callBacks;

    public CarConnectAdapterImpl() {
        callBacks = new Hashtable<>();
    }
    @Override
    public void registerCallBack(String key, CarConnectAdapterCallback callBack) {
        callBacks.put(key, callBack);
    }

    @Override
    public void initService() {
        msgPushService = (MsgPushService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MsgPushSingleServiceID);
        mBLAosService  = new BLAosService();
    }

    /**
     * 可连接设备请求
     * @param geoPoint 可连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public TaskResultBean requestMobileLinkable(GeoPoint geoPoint) {

        MobileLinkableRequest request = new MobileLinkableRequest();
        GsonUtils.copyBean(geoPoint, request);
        TaskResultBean result = new TaskResultBean();
        TaskResult result1;
        if (msgPushService != null) {
            result1 = msgPushService.request(request, this);
            GsonUtils.copyBean(result1, result);
            return  result;
        } else {
            Logger.d("MsgPush service is not initialized.");
            return new TaskResultBean();
        }
    }

    /**
     * 连接设备请求
     * @param deviceId 连接设备请求参数(不能为空)
     * @return 请求返回值
     */
    @Override
    public TaskResultBean requestMobileLink(long deviceId) {
        MobileLinkRequest request = new MobileLinkRequest();
        GsonUtils.copyBean(deviceId, request);
        TaskResultBean result = new TaskResultBean();
        TaskResult result1;
        if (msgPushService != null) {
            result1 = msgPushService.request(request, this);
            GsonUtils.copyBean(result1, result);
            return  result;
        }else {
            Logger.d("MsgPush service is not initialized.");
            return new TaskResultBean();
        }
    }

    /**
     * 终止所有请求
     */
    @Override
    public void abort() {
        if (msgPushService != null) {
            msgPushService.abort();
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
    public void abort(long taskId) {
        if (msgPushService != null) {
            msgPushService.abort(taskId);
            Logger.d("Request with taskId " + taskId + " aborted.");
        } else {
            Logger.d("MsgPush service is not initialized.");
        }
    }

    /**
     * 执行一个网络请求,车机互联的通用上报接口(除去导航上报)
     * @param pAosRequest 	网络请求, 内存由HMI管理 ,车机互联的通用上报接口(除去导航上报)
     * @return 整数 >0:网络请求的标识用于AbortRequest() ; =0:网络请求未发起，无回调。
     */
    @Override
    public long sendReqWsTserviceInternalLinkAutoReport(CarConnectRequestBaseBean pAosRequest) {

        GWsTserviceInternalLinkAutoReportRequestParam gWsTserviceInternalLinkAutoReportRequestParam = new GWsTserviceInternalLinkAutoReportRequestParam();
        GsonUtils.copyBean(pAosRequest, gWsTserviceInternalLinkAutoReportRequestParam);

        if (mBLAosService != null) {
            return mBLAosService.sendReqWsTserviceInternalLinkAutoReport(gWsTserviceInternalLinkAutoReportRequestParam, this);
        } else {
            Logger.d("BLAos service is not initialized.");
        }
        return -1;
    }

    @Override
    public void onResult(MobileLinkableResult result) {
        ThreadManager.getInstance().postUi(() -> {
            MobileLinkableResultBean mobileLinkableResultBean = new MobileLinkableResultBean();
            GsonUtils.copyBean(result, mobileLinkableResultBean);

            Logger.d("MobileLinkableResult: " + mobileLinkableResultBean.toString());

            for (CarConnectAdapterCallback callBack : callBacks.values()) {
                callBack.onMobileLinkableResult(mobileLinkableResultBean);
            }
        });
    }

    @Override
    public void onResult(MobileLinkResult result) {
        ThreadManager.getInstance().postUi(() -> {
            TaskResultBean taskResultBean = new TaskResultBean();
            GsonUtils.copyBean(result, taskResultBean);

            Logger.d("MobileLinkResult: " + taskResultBean.toString());

            for (CarConnectAdapterCallback callBack : callBacks.values()) {
                callBack.onMobileLinkResult(taskResultBean);
            }
        });
    }

    /**
     * 手机端推送发现可连接车机消息
     * @param msg 消息数据
     */
    @Override
    public void notifyMessage(MobileLinkPushMsg msg) {
        MsgPushItemBean mobileLinkPushMsgInfo = new MsgPushItemBean();
        GsonUtils.copyBean(msg, mobileLinkPushMsgInfo);

        Logger.d("MobileLinkPushMsgInfo: " + mobileLinkPushMsgInfo.toString());

        for (CarConnectAdapterCallback callBack : callBacks.values()) {
            callBack.notifyMobileLinkPushMessage(mobileLinkPushMsgInfo);
        }
    }

    /**
     * 网络库线程中回调业务应答类
     * @param gWsTserviceInternalLinkAutoReportResponseParam 车机互联的通用上报接口(除去导航上报)，应答类
     */
    @Override
    public void onRecvAck(GWsTserviceInternalLinkAutoReportResponseParam gWsTserviceInternalLinkAutoReportResponseParam) {
        final CarConnectResponseBaseBean responseBaseBean = new CarConnectResponseBaseBean();
        GsonUtils.copyBean(gWsTserviceInternalLinkAutoReportResponseParam, responseBaseBean);

        Logger.d("responseBaseBean: " + responseBaseBean.toString());

        for (CarConnectAdapterCallback callBack : callBacks.values()) {
            callBack.onRecvAckGWsTserviceInternalLinkAutoReportResponse(responseBaseBean);
        }
    }
}
