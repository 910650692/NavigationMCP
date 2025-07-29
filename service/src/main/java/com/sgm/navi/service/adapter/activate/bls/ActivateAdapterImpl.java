package com.sgm.navi.service.adapter.activate.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.activate.ActivateObserver;
import com.sgm.navi.service.adapter.activate.IActivateApi;
import com.sgm.navi.service.define.code.CodeManager;
import com.sgm.navi.service.greendao.CommonManager;

import java.util.ArrayList;
import java.util.List;


public class ActivateAdapterImpl implements IActivateApi {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    private List<ActivateObserver> mActObserverList;
    private ActivationManager.IActivateHelper manualActivateListener;
    private CodeManager codeManager;
    private static final String ERR_MSG = "Error Massage : ";

    public ActivateAdapterImpl() {
        Logger.e(TAG, "ActivateAdapterImpl init");
    }

    @Override
    public void startActivate() {
        Logger.e(TAG, "startActivate : ");
        onActivating();
        ActivationManager.getInstance().getThirdPartyUUID();
    }

    @Override
    public void init() {
        Logger.e(TAG, "init activate");
        mActObserverList = new ArrayList<>();
        codeManager = CodeManager.getInstance();

        manualActivateListener = new ActivationManager.IActivateHelper() {
            @Override
            public void onAppKeyGetFailed() {
                Logger.e(TAG, "appKey获取失败");
                onActivatedError(20010, codeManager.getActivateMsg(20010));
            }

            @Override
            public void onUuidGetFailed() {
                Logger.e(TAG, "uuid获取失败");
                onActivatedError(20005, codeManager.getActivateMsg(20005));
            }

            @Override
            public void onUUIDGet(final String uuid) {
                Logger.e(TAG, "uuid获取成功 : ", uuid);
                beginInitActivateService(uuid);
            }

            @Override
            public void onManualActivated(final boolean isSuccess) {
                if (!isSuccess) {
                    Logger.e(TAG, "手动激活失败");
                    onActivatedError(20009, codeManager.getActivateMsg(20009));
                    return;
                }
                Logger.e(TAG, "手动激活成功，开始BL初始化");
                onActivated();
            }

            @Override
            public void onOrderCreated(final boolean isSuccess) {
                if (!isSuccess) {
                    ActivationManager.setCreateOrderNum(ActivationManager.getCreateOrderNum() + 1);
                    final int times = ActivationManager.getCreateOrderNum();
                    Logger.e(TAG, "下单失败", times, "次");
                    if (times < 2) {
                        ActivationManager.getInstance().createCloudOrder();
                    } else {
                        onActivatedError(20006, codeManager.getActivateMsg(20006));
                    }
                    return;
                }
                startCheckOrder();
            }
        };
        final String uuid = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
        if (!ConvertUtils.isEmpty(uuid)) {
            Logger.e(TAG, "并非第一次启动，数据库UUID: ", uuid);
            if (!ActivationManager.getInstance().initActivationService(uuid)) {
                onActivatedError(20003, codeManager.getActivateMsg(20003));
            }
        }
    }

    @Override
    public void unInit() {
        ActivationManager.getInstance().unInit();
    }

    @Override
    public boolean checkActivation() {
        Logger.e(TAG, "checkActivation need to activate: ", BuildConfig.SDK_ACTIVATE);
        if (!BuildConfig.SDK_ACTIVATE) return true;
        return ActivationManager.getInstance().checkActivationStatus();
    }

    @Override
    public void addActivateObserver(final ActivateObserver observer) {
        ActivationManager.getInstance().addManualActivateListener(manualActivateListener);
        if (ConvertUtils.isContain(mActObserverList, observer)) {
            return;
        }
        mActObserverList.add(observer);
    }

    /**
     * 获取完uuid后开始后续流程
     *
     * @param uuid uuid
     */
    private void beginInitActivateService(final String uuid) {
        //初始化激活服务
        Logger.e(TAG, "uuid = ", uuid);
        if (!ActivationManager.getInstance().initActivationService(uuid)) {
            onActivatedError(20003, codeManager.getActivateMsg(20003));
            return;
        }
        //查询激活状态
        //自动检查激活文件是否存在，如果有会自动激活
        if (!ActivationManager.getInstance().checkActivationStatus()) {
            Logger.e(TAG, "无激活文件，或uuid不正确");
            //先查询订单
            if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID))) {
                Logger.e(TAG, "有订单号记录，直接查询订单");
                ActivationManager.getInstance().queryOrderStatus(new NetQueryManager.INetResultCallBack<QueryOrderResponse>() {
                    @Override
                    public void onSuccess(final QueryOrderResponse statusBean) {
                        Logger.e(TAG, "firstCheckOrderStatus success");
                        Logger.e(TAG, statusBean.toString());
                        manualActivate(statusBean.getMSerialNumber(), statusBean.getMActiveCode());
                    }

                    @Override
                    public void onFailed(final String errorCode) {
                        Logger.e(TAG, "firstCheckOrderStatus failed");
                        //首次查询没有订单后下单
                        if (ConvertUtils.equals(errorCode, "0003")) {
                            ActivationManager.getInstance().processAppKeyInvalid();
                            return;
                        }
                        ActivationManager.getInstance().createCloudOrder();
                    }
                });
            } else {
                ActivationManager.getInstance().createCloudOrder();
            }
        } else {
            Logger.e(TAG, "已经激活了");
            onActivated();
        }

    }

    @Override
    public void manualActivate(final String userCode, final String loginCode) {
        Logger.e(TAG, "manualActivate...");
        ActivationManager.getInstance().manualActivate(userCode, loginCode);
    }

    /**
     * 开始查询订单是否成功,
     */
    private void startCheckOrder() {
        if (!ActivationManager.getInstance().pollOrderStatusWithRetry()) {
            onActivatedError(20007, codeManager.getActivateMsg(20007));
        }
    }

    /**
     * 正在激活
     */
    private void onActivating() {
        for (ActivateObserver observer : mActObserverList) {
            observer.onActivating();
        }
    }

    /**
     * 网络激活成功回调
     */
    public void onActivated() {
        for (ActivateObserver observer : mActObserverList) {
            observer.onActivated();
        }
    }

    /**
     * 网络激活出现错误
     *
     * @param msg     错误信息
     * @param errCode 错误码
     */
    public void onActivatedError(final int errCode, final String msg) {
        Logger.e(TAG, "net status :", NetWorkUtils.Companion.getInstance().checkNetwork());
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            for (ActivateObserver observer : mActObserverList) {
                observer.onActivatedError(errCode, msg);
            }
        } else {
            for (ActivateObserver observer : mActObserverList) {
                observer.onActivatedError(20008, codeManager.getActivateMsg(20008));
            }
        }
    }

    @Override
    public String getAppKeyFromDB() {
        return CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.APP_KEY);
    }

    @Override
    public void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack) {
        ActivationManager.getInstance().getAppKeyFromNet(callBack);
    }

    @Override
    public String getUuidFromDB() {
        return CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
    }

    @Override
    public void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack) {
        ActivationManager.getInstance().getUuidFromNet(callBack);
    }
}
