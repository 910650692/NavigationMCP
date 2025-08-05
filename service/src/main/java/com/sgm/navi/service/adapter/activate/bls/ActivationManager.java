package com.sgm.navi.service.adapter.activate.bls;

import android.os.SystemProperties;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.activation.ActivationModule;
import com.autonavi.gbl.activation.model.ActivateReturnParam;
import com.autonavi.gbl.activation.model.ActivationInitParam;
import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.request.navibean.activate.AppKeyRequest;
import com.sgm.navi.patacnetlib.request.navibean.activate.CreateOrderRequest;
import com.sgm.navi.patacnetlib.request.navibean.activate.QueryOrderRequest;
import com.sgm.navi.patacnetlib.request.navibean.activate.UuidRequest;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.sgm.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

import java.util.Arrays;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public final class ActivationManager {
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    private static final String TEST_APP_ID = "SELF_DEVELOPED_MAP";
    private final static String RELEASE_STATUS = "ro.patac.production";
    private static String DEVICES_ID;
    private static String SYS_VERSION;
    private static String API_VERSION;
    private static String APP_KEY;
    private static String UUID;
    private static String ORDER_ID;
    private final static String SD = "SD";
    private static int CREATE_ORDER_NUM = 0;

    private IActivateHelper mActivateListener;
    private ActivationModule mActivationService;
    private boolean mIsInit = false;

    private ActivationManager() {
        DEVICES_ID = CalibrationPackage.getInstance().getDeviceId();
        Logger.e(TAG, "VIN = ", DEVICES_ID);
        final int releaseStatus = SystemProperties.getInt(RELEASE_STATUS, 0);
        Logger.e(TAG, "当前车机环境 = ", (releaseStatus == 1 ? "生产环境" : "测试环境"));
    }

    /**
     * 重置变量
     */
    private void resetVar() {
        CREATE_ORDER_NUM = 0;
        SYS_VERSION = "1.0";
        API_VERSION = "1.0";
        DEVICES_ID = "";
        APP_KEY = "";
        UUID = "";
        ORDER_ID = "";
    }

    public void init() {
        resetVar();
        mActivationService = ActivationModule.getInstance();
        DEVICES_ID = CalibrationPackage.getInstance().getDeviceId();
        Logger.e(TAG, "ActivationManager: devicesId = ", DEVICES_ID);
        Logger.e(TAG, "                  sysVersion = ", SYS_VERSION);
        final int releaseStatus = SystemProperties.getInt(RELEASE_STATUS, 0);
        Logger.e(TAG, "当前车机环境 = ", (releaseStatus == 1 ? "生产环境" : "测试环境"));
    }

    /**
     * 反初始化
     */
    public void unInit() {
        if (!mIsInit) {
            return;
        }
        Logger.e(TAG, "unInit: ");
        mActivationService.setNetActivateObserver(null);
        mActivationService.unInit();
        mActivateListener = null;
    }

    /**
     * 添加手动激活成功回调
     *
     * @param listener listener
     */
    public void addManualActivateListener(final IActivateHelper listener) {
        mActivateListener = listener;
    }

    public static ActivationManager getInstance() {
        return Helper.INSTANCE;
    }

    private static final class Helper {
        private static final ActivationManager INSTANCE = new ActivationManager();
    }

    /**
     * 获取三方UUID
     */
    public void getThirdPartyUUID() {
        Logger.e(TAG, "getThirdPartyUUID: ");
        if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.APP_KEY))) {
            APP_KEY = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.APP_KEY);
            Logger.e(TAG, "数据库存有APP_KEY = ", APP_KEY);
            NetQueryManager.getInstance().saveAppSecurity(APP_KEY);
            postUUID();
            return;
        }
        genAppKey();
    }

    /**
     * appKey生成
     */
    private void genAppKey() {
        Logger.e(TAG, "genAppKey: ");
        getAppKeyFromNet(new NetQueryManager.INetResultCallBack<AppKeyResponse>() {
            @Override
            public void onSuccess(final AppKeyResponse response) {
                Logger.e(TAG, response.toString());
                APP_KEY = response.getMAppKey();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.APP_KEY, APP_KEY);
                NetQueryManager.getInstance().saveAppSecurity(APP_KEY);
                postUUID();
            }

            @Override
            public void onFailed(final String errorCode) {
                Logger.e(TAG, "AppKey请求失败");
                mActivateListener.onAppKeyGetFailed();
            }
        });
    }

    /**
     * 网络请求uuid
     */
    private void postUUID() {
        Logger.e(TAG, "postUUID: ");

        if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY))) {
            UUID = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
            Logger.e(TAG, "数据库存有UUID = ", UUID);
            mActivateListener.onUUIDGet(UUID);
            return;
        }

        getUuidFromNet(new NetQueryManager.INetResultCallBack<UuidResponse>() {
            @Override
            public void onSuccess(final UuidResponse response) {
                Logger.e(TAG, response.toString());
                UUID = response.getMVin();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.UUID_KEY, UUID);
                mActivateListener.onUUIDGet(response.getMVin());
            }

            @Override
            public void onFailed(final String errorCode) {
                Logger.e(TAG, "Uuid请求失败");
                if (ConvertUtils.equals(errorCode, "0003")) {
                    processAppKeyInvalid();
                    return;
                }
                mActivateListener.onUuidGetFailed();
            }
        });
    }

    /**
     * 初始化激活服务
     *
     * @param uuid uuid
     * @return 是否成功
     */
    public boolean initActivationService(final String uuid) {
        Logger.e(TAG, "initActivationService: ");

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
        Logger.e(TAG, "actInitParam.szUserDataFileDir = ", actInitParam.szUserDataFileDir);

        if (null == mActivationService) mActivationService = ActivationModule.getInstance();
        final int initResult = mActivationService.init(actInitParam);
        Logger.e(TAG, "initActivateParam: initResult = ", initResult);
        final boolean initSuccess = ConvertUtils.equals(0, initResult);
        mIsInit = initSuccess;
        return initSuccess;
    }

    /**
     * 检查激活状态
     *
     * @return 是否成功
     */
    public boolean checkActivationStatus() {
        if (mActivationService == null) {
            Logger.e(TAG, "mActivationService == null");
            return false;
        }
        if (!mIsInit) {
            Logger.e(TAG, "mActivationService 未初始化");
            return false;
        }

        final int activateStatus = mActivationService.getActivateStatus();
        Logger.e(TAG, "激活状态码 = ", activateStatus);
        return ConvertUtils.equals(0, activateStatus);
    }

    /**
     * 云对云下单
     */
    public void createCloudOrder() {
        Logger.e(TAG, "createCloudOrder");
        final String uuid = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
        if (ConvertUtils.isEmpty(uuid)) {
            Logger.e(TAG, "uuid信息缺失");
            postUUID();
            return;
        } else {
            Logger.e(TAG, "UUID in database = ", uuid);
            UUID = uuid;
        }

        final CreateOrderRequest createOrderReq = new CreateOrderRequest(API_VERSION, TEST_APP_ID, UUID, SD);
        Logger.e(TAG, "createOrderReq  : ", createOrderReq);
        NetQueryManager.getInstance().createOrder(createOrderReq, new NetQueryManager.INetResultCallBack<CreateOrderResponse>() {
            @Override
            public void onSuccess(final CreateOrderResponse response) {
                Logger.e(TAG, response.toString());
                ORDER_ID = response.getMCusOrderId();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID, response.getMCusOrderId());
                mActivateListener.onOrderCreated(true);
            }

            @Override
            public void onFailed(final String errorCode) {
                Logger.e(TAG, "下单请求失败");
                if (ConvertUtils.equals(errorCode, "0003")) {
                    processAppKeyInvalid();
                    return;
                }
                mActivateListener.onOrderCreated(false);
            }
        });

    }


    /**
     * 带重试的订单激活状态查询
     *
     * @return 返回是否成功
     */
    public boolean pollOrderStatusWithRetry() {
        final ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);
        final CompletableFuture<Boolean> future = new CompletableFuture<>();
        final AtomicInteger retryCount = new AtomicInteger(0);
        final int maxRetries = 3;
        final long[] delays = {
                AutoMapConstant.DELAY_SEVENTY_SECOND,
                AutoMapConstant.DELAY_SEVENTY_SECOND,
                AutoMapConstant.DELAY_SEVENTY_SECOND
        };
        final AtomicReference<Runnable> taskRef = new AtomicReference<>();

        final Runnable task = new Runnable() {
            @Override
            public void run() {
                if (future.isDone()) {
                    return;
                }
                try {
                    final NetQueryManager.INetResultCallBack<QueryOrderResponse> callBack = new NetQueryManager.INetResultCallBack<QueryOrderResponse>() {
                        @Override
                        public void onSuccess(final QueryOrderResponse statusBean) {
                            Logger.e(TAG, statusBean.toString());
                            if (ConvertUtils.equals(statusBean.getMOrderStatus(), "2")) {
                                future.complete(true);
                                executor.shutdownNow();
                                manualActivate(statusBean.getMSerialNumber(), statusBean.getMActiveCode());
                            } else if (ConvertUtils.equals(statusBean.getMOrderStatus(), "1")) {
                                Logger.e(TAG, "已下单/等待交付");
                                final int currentCount = retryCount.incrementAndGet();
                                Logger.e(TAG, "轮询次数 : ", currentCount);
                                if (currentCount > maxRetries) {
                                    future.complete(false);
                                    executor.shutdownNow();
                                } else {
                                    final int delayIndex = Math.min(currentCount - 1, delays.length - 1);
                                    executor.schedule(taskRef.get(), delays[delayIndex], TimeUnit.SECONDS);
                                }
                            } else if (ConvertUtils.equals(statusBean.getMOrderStatus(), "3")) {
                                Logger.e(TAG, "下单失败");
                                future.complete(false);
                                executor.shutdownNow();
                                mActivateListener.onOrderCreated(false);
                            }
                        }

                        @Override
                        public void onFailed(final String errorCode) {
                            Logger.e(TAG, "查询订单网络请求失败");
                            if (ConvertUtils.equals(errorCode, "0003")) {
                                future.complete(false);
                                executor.shutdownNow();
                                processAppKeyInvalid();
                                return;
                            }
                            final int currentCount = retryCount.incrementAndGet();
                            Logger.e(TAG, "轮询次数 : ", currentCount);
                            if (currentCount > maxRetries) {
                                future.complete(false);
                                executor.shutdownNow();
                                mActivateListener.onOrderCreated(false);
                            } else {
                                final int delayIndex = Math.min(currentCount - 1, delays.length - 1);
                                executor.schedule(taskRef.get(), delays[delayIndex], TimeUnit.SECONDS);
                            }
                        }
                    };
                    getInstance().queryOrderStatus(callBack);

                } catch (NullPointerException | IllegalArgumentException e) {
                    future.completeExceptionally(e);
                    executor.shutdownNow();
                }
            }
        };
        taskRef.set(task);
        executor.schedule(task, 5, TimeUnit.SECONDS);

        try {
            // 总超时
            final long totalTimeout = Arrays.stream(delays).sum() + 30; // 70 * 3 + 30 = 240 seconds
            return future.get(totalTimeout, TimeUnit.SECONDS);
        } catch (CancellationException e) {
            executor.shutdownNow();
            Logger.e(TAG, "CancellationException");
            return false;
        } catch (TimeoutException e) {
            executor.shutdownNow();
            Logger.e(TAG, "订单状态轮询总超时");
            return false;
        } catch (ExecutionException e) {
            Logger.e(TAG, "轮询异常: ", e.getCause());
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Logger.e(TAG, "轮询被中断");
            return false;
        } finally {
            executor.shutdown();
        }
    }

    /**
     * 查询订单状态
     *
     * @param callBack 网络结果回调
     */
    public void queryOrderStatus(final NetQueryManager.INetResultCallBack<QueryOrderResponse> callBack) {
        Logger.e(TAG, "checkOrderStatus");
        final String orderId = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID);

        if (ConvertUtils.isEmpty(ORDER_ID) && ConvertUtils.isEmpty(orderId)) {
            Logger.e(TAG, "订单id信息缺失");
            return;
        }

        if (!ConvertUtils.equals(ORDER_ID, orderId) && !ConvertUtils.isEmpty(orderId)) {
            Logger.e(TAG, "ORDER_ID constant = ", ORDER_ID, " ; orderId in database = ", orderId);
            ORDER_ID = orderId;
        }
        Logger.e(TAG, "ORDER_ID = ", ORDER_ID);
        final QueryOrderRequest queryOrderRequest = new QueryOrderRequest(SYS_VERSION, TEST_APP_ID, UUID, ORDER_ID);
        Logger.e(TAG, "queryOrderRequest  : ", queryOrderRequest);
        NetQueryManager.getInstance().queryOrder(queryOrderRequest, callBack);

    }

    /**
     * 手动激活
     *
     * @param userCode  序列号
     * @param loginCode 激活码
     */
    public void manualActivate(final String userCode, final String loginCode) {
        Logger.e(TAG, "userCode = ", userCode, "; loginCode = ", loginCode);
        if (mActivationService == null) {
            Logger.e(TAG, "mActivationService == null");
            return;
        }
        if (!mIsInit) {
            Logger.e(TAG, "mActivationService 未初始化");
            return;
        }
        final ActivateReturnParam activateReturnParam = mActivationService.manualActivate(userCode, loginCode);
        if (activateReturnParam == null) {
            Logger.e(TAG, "activateReturnParam == null");
            return;
        }
        Logger.e(TAG, "activateReturnParam.iErrorCode = ", activateReturnParam.iErrorCode);
        mActivateListener.onManualActivated(ConvertUtils.equals(0, activateReturnParam.iErrorCode));
    }

    public void processAppKeyInvalid() {
        Logger.e(TAG, "错误码 0003 AppKey失效，重新请求");
        genAppKey();
    }

    /**
     * 从网络获取AppKey
     *
     * @param callBack 网络结果回调
     */
    public void getAppKeyFromNet(final NetQueryManager.INetResultCallBack<AppKeyResponse> callBack) {
        final AppKeyRequest req = new AppKeyRequest(API_VERSION);
        NetQueryManager.getInstance().queryAppKey(req, callBack);
    }

    /**
     * 从网络获取Uuid
     *
     * @param callBack 网络结果回调
     */
    public void getUuidFromNet(final NetQueryManager.INetResultCallBack<UuidResponse> callBack) {
        final UuidRequest uuidRequest = new UuidRequest(API_VERSION, TEST_APP_ID, SYS_VERSION, DEVICES_ID);
        Logger.e(TAG, "uuid req : ", uuidRequest);
        NetQueryManager.getInstance().queryUuid(uuidRequest, callBack);
    }

    public static int getCreateOrderNum() {
        return CREATE_ORDER_NUM;
    }

    public static void setCreateOrderNum(final int createOrderNum) {
        CREATE_ORDER_NUM = createOrderNum;
    }


    public interface IActivateHelper {

        /**
         * appKey获取失败回调
         */
        void onAppKeyGetFailed();

        /**
         * uuid获取失败
         */
        void onUuidGetFailed();

        /**
         * uuid获取后回调
         *
         * @param uuid uuid
         */
        void onUUIDGet(final String uuid);

        /**
         * 手动激活结果回调给impl
         *
         * @param isSuccess 是否成功
         */
        void onManualActivated(final boolean isSuccess);

        /**
         * 开始下单回调给impl
         *
         * @param isSuccess 是否成功
         */
        void onOrderCreated(final boolean isSuccess);
    }
}