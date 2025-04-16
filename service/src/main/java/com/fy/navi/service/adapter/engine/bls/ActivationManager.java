package com.fy.navi.service.adapter.engine.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.activation.ActivationModule;
import com.autonavi.gbl.activation.model.ActivationInitParam;
import com.autonavi.gbl.activation.observer.INetActivateObserver;
import com.fy.navi.service.GBLCacheFilePath;

public final class ActivationManager {
    private static final String TAG = "ActivationManager";

    private final ActivationModule mActivationService;
    private boolean mIsInit = false;

    private final INetActivateObserver mNetActivateObserver = new INetActivateObserver() {
        @Override
        public void onNetActivateResponse(final int returnCode) {
            //网络激活结果处理
            Logger.i(TAG, "网络激活返回码 = " + returnCode);
        }
    };

    private ActivationManager() {
        Logger.d(TAG, "ActivationManager: ");
        mActivationService = ActivationModule.getInstance();
    }

    /**
     * 反初始化
     */
    public void unInit() {
        if (!mIsInit) {
            return;
        }
        Logger.d(TAG, "unInit: ");
        mActivationService.setNetActivateObserver(null);
        mActivationService.unInit();
    }

    public static ActivationManager getInstance() {
        return Helper.INSTANCE;
    }

    private static final class Helper {
        private static final ActivationManager INSTANCE = new ActivationManager();
    }

    /**
     * 获取三方UUID
     * @return uuid
     */
    public String getThirdPartyUUID() {
        Logger.d(TAG, "getThirdPartyUUID: ");
        // todo调用三方SDK接口
        return "uuid-from-third-party";
    }

    /**
     * 初始化激活服务
     * @param uuid uuid
     * @return 是否成功
     */
    public boolean initActivationService(final String uuid) {
        Logger.d(TAG, "initActivationService: ");
        mActivationService.setNetActivateObserver(mNetActivateObserver);

        final ActivationInitParam actInitParam = new ActivationInitParam();

        // 是否检查客户编号
        actInitParam.isCheckClientNo = true;
        // 是否检查项目编号
        actInitParam.isCheckModelNo = true;
        // 是否支持 批量激活
        actInitParam.isSupportVolumeAct = false;
        //项目编号 非基础版项目传0, 内部读取配置文件的值
        actInitParam.iProjectId = 0;
        // 激活码的长度为24，设置为其他值无法激活
        actInitParam.iCodeLength = 24;
        // 设备编号 info4的uuid
        actInitParam.szDeviceID = uuid;
        // 激活文件保存路径
        actInitParam.szUserDataFileDir = GBLCacheFilePath.ACTIVATE_USER_DATA;

        final int initResult = mActivationService.init(actInitParam);
        Logger.i(TAG, "initActivateParam: initResult = " + initResult);
        final boolean initSuccess = ConvertUtils.equals(0, initResult);
        mIsInit = initSuccess;
        return initSuccess;
    }

    /**
     * 检查激活状态
     * @return 是否成功
     */
    public boolean checkActivationStatus() {
        if (mActivationService == null) {
            Logger.e(TAG, "mActivationService == null" );
            return false;
        }
        final int activateStatus = mActivationService.getActivateStatus();
        Logger.e(TAG, "激活状态码 = " + activateStatus);
        return ConvertUtils.equals(0, activateStatus);
    }

    /**
     * 云对云下单
     * @return 是否成功
     */
    public boolean createCloudOrder() {
        Logger.e(TAG, "createCloudOrder" );
        // 调用三方下单接口
        return true;
    }

    /**
     * 查询订单状态
     * @return 是否成功
     */
    public boolean checkOrderStatus() {
        Logger.e(TAG, "checkOrderStatus" );
        // 模拟随机返回结果
        return false;
    }

    /**
     * 网络激活
     * @return 是否成功
     */
    public boolean netActivate() {
        final String hardWareCode = "0000000000"; // 默认10个0
        final int netActivateResult = mActivationService.netActivate(hardWareCode);
        Logger.i(TAG, "netActivateResult = " + netActivateResult);
        return ConvertUtils.equals(0, netActivateResult);
    }
}