package com.fy.navi.service.adapter.activate.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.patacnetlib.NetQueryManager;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.activate.ActivateObserver;
import com.fy.navi.service.adapter.activate.IActivateApi;
import com.fy.navi.service.define.code.CodeManager;
import com.fy.navi.service.greendao.CommonManager;

import java.util.ArrayList;
import java.util.List;


public class ActivateAdapterImpl implements IActivateApi {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    private final List<ActivateObserver> mActObserverList;
    private final ActivationManager.IActivateHelper manualActivateListener;
    private static final String ERR_MSG = "Error Massage : ";

    public ActivateAdapterImpl() {
        mActObserverList = new ArrayList<>();
        manualActivateListener = new ActivationManager.IActivateHelper() {
            @Override
            public void onUUIDGet(final String uuid) {
                Logger.d(TAG, "uuid获取成功 : " + uuid);
                if (ConvertUtils.isEmpty(uuid)) {
                    logResult(20005);
                    onActivatedError();
                    return;
                }
                beginInitActivateService(uuid);
            }

            @Override
            public void onManualActivated(final boolean isSuccess) {
                if (!isSuccess) {
                    Logger.e(TAG, "手动激活失败");
                    return;
                }
                Logger.d(TAG, "手动激活成功，开始BL初始化");
                onActivated();
            }

            @Override
            public void onNetActivated(final boolean isSuccess) {
            }

            @Override
            public void onOrderCreated(final boolean isSuccess) {
                if (!isSuccess) {
                    if (ActivationManager.getQueryOrderNum() < 3) {
                        ActivationManager.getInstance().createCloudOrder();
                    } else {
                        logResult(20006);
                        onActivatedError();
                    }
                    return;
                }
                startCheckOrder();
            }
        };
        ActivationManager.getInstance().addManualActivateListener(manualActivateListener);
    }

    @Override
    public void initActivate() {
        startActivate();
    }

    @Override
    public void unInit() {
        ActivationManager.getInstance().unInit();
    }

    @Override
    public boolean checkActivation() {
        return true;
        //return ActivationManager.getInstance().checkActivationStatus();
    }

    @Override
    public void addActivateObserver(final ActivateObserver observer) {
        if (ConvertUtils.isContain(mActObserverList, observer)) {
            return;
        }
        mActObserverList.add(observer);
    }

    /**
     * 开始激活流程
     */
    public void startActivate() {
        Logger.d(TAG, "startActivate : ");
        onActivating();
        ActivationManager.getInstance().getThirdPartyUUID();
        //beginInitActivateService("123");
    }

    /**
     * 重试网络激活
     */
    @Override
    public void netActivateRetry() {
    }


    /**
     * 获取完uuid后开始后续流程
     *
     * @param uuid uuid
     */
    private void beginInitActivateService(final String uuid) {
        //初始化激活服务
        Logger.d(TAG, "uuid = " + uuid);
        if (!ActivationManager.getInstance().initActivationService(uuid)) {
            logResult(20003);
            onActivatedError();
            return;
        }
        //查询激活状态
        //自动检查激活文件是否存在，如果有会自动激活
        if (!ActivationManager.getInstance().checkActivationStatus()) {
            Logger.d(TAG, "无激活文件，或uuid不正确");
            //先查询订单
            if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID))) {
                Logger.d(TAG, "有订单号记录，直接查询订单");
                ActivationManager.getInstance().queryOrderStatus(new NetQueryManager.INetResultCallBack<QueryOrderResponse>() {
                    @Override
                    public void onSuccess(final QueryOrderResponse statusBean) {
                        Logger.d(TAG, "firstCheckOrderStatus success");
                        Logger.d(TAG, statusBean.toString());
                        manualActivate(statusBean.getSerialNumber(), statusBean.getActiveCode());
                    }

                    @Override
                    public void onFailed() {
                        Logger.d(TAG, "firstCheckOrderStatus failed");
                        //首次查询没有订单后下单
                        ActivationManager.getInstance().createCloudOrder();
                    }
                });
            } else {
                ActivationManager.getInstance().createCloudOrder();
            }
        } else {
            onActivated();
        }

    }

    @Override
    public void manualActivate(final String userCode, final String loginCode) {
        Logger.d(TAG, "manualActivate...");
        ActivationManager.getInstance().manualActivate(userCode, loginCode);
    }

    /**
     * 开始查询订单是否成功,
     */
    private void startCheckOrder() {
        if (!ActivationManager.getInstance().pollOrderStatusWithRetry()) {
            logResult(20007);
            return;
        }
    }

    /**
     * 正在激活
     */
    private void onActivating() {
        for (ActivateObserver observer : mActObserverList) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    observer.onActivating();
                }
            });
        }
    }

    /**
     * 网络激活成功回调
     */
    public void onActivated() {
        for (ActivateObserver observer : mActObserverList) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    observer.onActivated();
                }
            });
        }
    }

    /**
     * 网络激活出现错误
     */
    public void onActivatedError() {
        for (ActivateObserver observer : mActObserverList) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    observer.onActivatedError();
                }
            });
        }
    }

    /**
     * 封装错误数据构建
     *
     * @param code 错误码
     */
    private void logResult(final int code) {
        Logger.d(TAG, ERR_MSG + "  " + code + " :" + CodeManager.getActivateMsg(code));
    }
}
