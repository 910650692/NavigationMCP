package com.fy.navi.service.adapter.activate.bls;

import android.os.SystemProperties;


import com.android.utils.ConvertUtils;
import com.android.utils.DevicesIdUtil;
import com.android.utils.log.Logger;
import com.autonavi.gbl.activation.ActivationModule;
import com.autonavi.gbl.activation.model.ActivateReturnParam;
import com.autonavi.gbl.activation.model.ActivationInitParam;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.fy.navi.patacnetlib.NetQueryManager;
import com.fy.navi.patacnetlib.request.navibean.activate.AppKeyRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.CreateOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.QueryOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.UuidRequest;
import com.fy.navi.patacnetlib.response.activate.AppKeyResponse;
import com.fy.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.patacnetlib.response.activate.UuidResponse;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.greendao.CommonManager;

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
    private static int QUERY_ORDER_NUM = 0;

    private final static String EXCEPTION_CODE = "Exception code : ";
    private final static String EXCEPTION_MSG = "Exception msg : ";


    private IActivateHelper mActivateListener;
    private final ActivationModule mActivationService;
    private boolean mIsInit = false;

    public boolean testFlag = false;

    private ActivationManager() {
        resetVar();
        mActivationService = ActivationModule.getInstance();
        DEVICES_ID = DevicesIdUtil.getInstance().getDeviceId();
        Logger.d(TAG, "ActivationManager: devicesId = " + DEVICES_ID);
        Logger.d(TAG, "                  sysVersion = " + SYS_VERSION);

        final int releaseStatus = SystemProperties.getInt(RELEASE_STATUS, 0);
        Logger.d(TAG, "当前车机环境 : "
                + releaseStatus + " : " + (releaseStatus == 1 ? "生产环境" : "测试环境"));
    }

    /**
     * 重置变量
     */
    private void resetVar() {
        QUERY_ORDER_NUM = 0;
        SYS_VERSION = "1.0";
        API_VERSION = "1.0";
        DEVICES_ID = "";
        APP_KEY = "";
        UUID = "";
        ORDER_ID = "";
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
        Logger.d(TAG, "getThirdPartyUUID: ");
        genAppKey();
    }

    /**
     * appKey生成
     */
    private void genAppKey() {
        Logger.d(TAG, "genAppKey: ");

        if (!ConvertUtils.isEmpty(APP_KEY)) {
            Logger.d(TAG, "APP_KEY静态变量不为空 = " + APP_KEY);
            NetQueryManager.getInstance().saveAppSecurity(APP_KEY);
            testFlag = true;
            //mActivateListener.onNetActivated(true);
            postUUID();
            return;
        }

        if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.APP_KEY))) {
            APP_KEY = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.APP_KEY);
            Logger.d(TAG, "APP_KEY静态变量为空，数据库存有APP_KEY = " + APP_KEY);
            NetQueryManager.getInstance().saveAppSecurity(APP_KEY);
            testFlag = true;
            //mActivateListener.onNetActivated(true);
            postUUID();
            return;
        }

        final AppKeyRequest req = new AppKeyRequest(API_VERSION);
        NetQueryManager.getInstance().queryAppKey(req, new NetQueryManager.INetResultCallBack<AppKeyResponse>() {
            @Override
            public void onSuccess(final AppKeyResponse response) {
                Logger.d(TAG, response.toString());
                APP_KEY = response.getMAppKey();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.APP_KEY, APP_KEY);
                NetQueryManager.getInstance().saveAppSecurity(APP_KEY);
                testFlag = true;
                //mActivateListener.onNetActivated(true);
                postUUID();
            }

            @Override
            public void onFailed() {
                Logger.d(TAG, "AppKey请求失败");
            }
        });
    }

    /**
     * 网络请求uuid
     */
    private void postUUID() {
        Logger.d(TAG, "postUUID: ");

        if (!ConvertUtils.isEmpty(UUID)) {
            Logger.d(TAG, "UUID静态变量不为空 = " + UUID);
            mActivateListener.onUUIDGet(UUID);
            return;
        }

        if (!ConvertUtils.isEmpty(CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY))) {
            UUID = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
            Logger.d(TAG, "UUID静态变量为空，数据库存有UUID = " + UUID);
            mActivateListener.onUUIDGet(UUID);
            return;
        }

        final UuidRequest uuidRequest = new UuidRequest(API_VERSION, TEST_APP_ID, SYS_VERSION, DEVICES_ID);
        Logger.d(TAG, "uuid req : " + uuidRequest);

        NetQueryManager.getInstance().queryUuid(uuidRequest, new NetQueryManager.INetResultCallBack<UuidResponse>() {
            @Override
            public void onSuccess(final UuidResponse response) {
                Logger.d(TAG, response.toString());
                UUID = response.getMVin();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.UUID_KEY, UUID);
                mActivateListener.onUUIDGet(response.getMVin());
            }

            @Override
            public void onFailed() {
                Logger.d(TAG, "Uuid请求失败");
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
        Logger.d(TAG, "initActivationService: ");

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
     *
     * @return 是否成功
     */
    public boolean checkActivationStatus() {
        if (mActivationService == null) {
            Logger.d(TAG, "mActivationService == null");
            return false;
        }
        if (!mIsInit) {
            Logger.d(TAG, "mActivationService 未初始化");
            return false;
        }
//        final long limitTime = ServiceMgr.getServiceMgrInstance().getSdkLimitTimeUTC();
//        if (limitTime == 0) {
//            Logger.e(TAG, "无效的SDK");
//        }
//        final long limitTimeMillis = limitTime / 1000;
//        final long currentTime = System.currentTimeMillis();
//        Logger.d(TAG, "limitTimeMillis = {" + limitTimeMillis + "}， currentTime = {" + currentTime + "}");
//        if (limitTimeMillis < currentTime) {
//            Logger.e(TAG, "SDK过期需要激活");
//        }

        final int activateStatus = mActivationService.getActivateStatus();
        Logger.d(TAG, "激活状态码 = " + activateStatus);
        return ConvertUtils.equals(0, activateStatus);
    }

    /**
     * 云对云下单
     */
    public void createCloudOrder() {
        Logger.d(TAG, "createCloudOrder");
        final String uuid = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.UUID_KEY);
        if (ConvertUtils.isEmpty(UUID) && ConvertUtils.isEmpty(uuid)) {
            Logger.e(TAG, "uuid信息缺失");
            postUUID();
            return;
        }

        if (!ConvertUtils.equals(UUID, uuid) && !ConvertUtils.isEmpty(uuid)) {
            Logger.d(TAG, "UUID constant = " + UUID + " ; UUID in database = " + uuid);
            UUID = uuid;
        }
        Logger.i(TAG, "UUID = " + UUID);
        final CreateOrderRequest createOrderReq = new CreateOrderRequest(API_VERSION, TEST_APP_ID, UUID, SD);
        Logger.d(TAG, "createOrderReq  : " + createOrderReq);
        NetQueryManager.getInstance().createOrder(createOrderReq, new NetQueryManager.INetResultCallBack<CreateOrderResponse>() {
            @Override
            public void onSuccess(final CreateOrderResponse response) {
                Logger.d(TAG, response.toString());
                ORDER_ID = response.getMCusOrderId();
                CommonManager.getInstance().insertOrReplace(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID, response.getMCusOrderId());
                mActivateListener.onOrderCreated(true);
            }

            @Override
            public void onFailed() {
                ++QUERY_ORDER_NUM;
                Logger.e(TAG, "下单失败" + QUERY_ORDER_NUM + "次");
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
                AutoMapConstant.DELAY_MINUTE * 5,
                AutoMapConstant.DELAY_MINUTE * 10,
                AutoMapConstant.DELAY_MINUTE * 10
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
                            Logger.d(TAG, "查询订单网络请求成功");
                            Logger.d(TAG, statusBean.toString());
                            if (ConvertUtils.equals(statusBean.getMOrderStatus(), "2")) {
                                future.complete(true);
                                executor.shutdownNow();
                                manualActivate(statusBean.getMSerialNumber(), statusBean.getMActiveCode());
                            } else {
                                Logger.d(TAG, "返回订单未成功");
                                final int currentCount = retryCount.incrementAndGet();
                                Logger.d(TAG, "轮询次数 : " + currentCount);
                                if (currentCount > maxRetries) {
                                    future.complete(false);
                                    executor.shutdownNow();
                                } else {
                                    executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.SECONDS);
                                }
                            }
                        }

                        @Override
                        public void onFailed() {
                            Logger.d(TAG, "查询订单网络请求失败");

                            final int currentCount = retryCount.incrementAndGet();
                            Logger.d(TAG, "轮询次数 : " + currentCount);
                            if (currentCount > maxRetries) {
                                future.complete(false);
                                executor.shutdownNow();
                            } else {
                                executor.schedule(taskRef.get(), delays[currentCount], TimeUnit.SECONDS);
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
        executor.schedule(task, delays[0], TimeUnit.SECONDS);

        try {
            // 总超时 = 所有可能延迟之和 + 缓冲时间（例如15分钟）
            final long totalTimeout = Arrays.stream(delays).sum() + 10; // 2+5+5 +1=13分钟
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
            Logger.e(TAG, "轮询异常: " + e.getCause());
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
        Logger.d(TAG, "checkOrderStatus");
        final String orderId = CommonManager.getInstance().getValueByKey(AutoMapConstant.ActivateOrderTAG.SD_ORDER_ID);

        if (ConvertUtils.isEmpty(ORDER_ID) && ConvertUtils.isEmpty(orderId)) {
            Logger.e(TAG, "订单id信息缺失");
            return;
        }

        if (!ConvertUtils.equals(ORDER_ID, orderId) && !ConvertUtils.isEmpty(orderId)) {
            Logger.d(TAG, "ORDER_ID constant = " + ORDER_ID + " ; orderId in database = " + orderId);
            ORDER_ID = orderId;
        }
        Logger.i(TAG, "ORDER_ID = " + ORDER_ID);
        final QueryOrderRequest queryOrderRequest = new QueryOrderRequest(SYS_VERSION, TEST_APP_ID, UUID, ORDER_ID);
        Logger.d(TAG, "queryOrderRequest  : " + queryOrderRequest);
        NetQueryManager.getInstance().queryOrder(queryOrderRequest, callBack);

    }

    /**
     * 网络激活
     */
    public void netActivate() {
        //++NET_FAILED_COUNT;
        //final String hardWareCode = "0000000000"; // 默认10个0
        //final int netActivateResult = mActivationService.netActivate(hardWareCode);
        //Logger.i(TAG, "netActivateResult = " + netActivateResult + "; failed count : " + NET_FAILED_COUNT);
        mActivateListener.onNetActivated(false);
    }
/*
这是激活需要的参数信息
设备号：22A5E69D7783D0A06C62BB7BE9A73ADA(001042)

序列号：PETAW9KTKQS79GZQBCXA984B

激活码：UR32YH4SP4S9SQ57SJZ9ZUK9

渠道号:C13953968867
 */

    /**
     * 手动激活
     *
     * @param userCode  序列号
     * @param loginCode 激活码
     */
    public void manualActivate(final String userCode, final String loginCode) {
        Logger.d(TAG, "userCode = " + userCode + "; loginCode = " + loginCode);
        if (mActivationService == null) {
            Logger.d(TAG, "mActivationService == null");
            return;
        }
        if (!mIsInit) {
            Logger.d(TAG, "mActivationService 未初始化");
            return;
        }
        final ActivateReturnParam activateReturnParam = mActivationService.manualActivate("PETAW9KTKQS79GZQBCXA984B", "UR32YH4SP4S9SQ57SJZ9ZUK9");
        //final ActivateReturnParam activateReturnParam = mActivationService.manualActivate(userCode, loginCode);
        if (activateReturnParam == null) {
            Logger.d(TAG, "activateReturnParam == null");
            return;
        }
        Logger.d(TAG, "activateReturnParam.iErrorCode = " + activateReturnParam.iErrorCode);
        mActivateListener.onManualActivated(ConvertUtils.equals(0, activateReturnParam.iErrorCode));
    }

    public static int getQueryOrderNum() {
        return QUERY_ORDER_NUM;
    }

    public static void setQueryOrderNum(final int queryOrderNum) {
        QUERY_ORDER_NUM = queryOrderNum;
    }


    public interface IActivateHelper {

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
         * 网络激活回调impl
         *
         * @param isSuccess 是否成功
         */
        void onNetActivated(final boolean isSuccess);

        /**
         * 开始下单回调给impl
         *
         * @param isSuccess 是否成功
         */
        void onOrderCreated(final boolean isSuccess);
    }
}