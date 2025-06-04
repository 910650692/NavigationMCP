package com.fy.navi.adas;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.adas.bean.OddBean;
import com.fy.navi.fsa.R;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.navi.PlayModule;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.l2.L2InfoCallback;
import com.fy.navi.service.logicpaket.l2.L2Package;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.signal.SignalCallback;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.DataCallback;
import com.gm.cn.adassdk.DataType;
import com.gm.cn.adassdk.automateddrivingux.Properties;
import com.gm.cn.adassdk.automateddrivingux.PropertyCallback;
import com.gm.cn.adassdk.proto.ADUProto;
import com.google.protobuf.InvalidProtocolBufferException;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

/**
 * L2++ 管理类
 */
public final class L2PPManager {
    //本类TAG
    private static final String TAG = L2PPManager.class.getSimpleName();

    /**
     * 车端高级辅助驾驶系统 管理类
     */
    private AdasManager mAdasManager;
    /**
     * 是否初始化
     */
    private boolean mInitialized = false;

    /**
     * 获取L2PPManager实例
     *
     * @return
     */
    public static L2PPManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    /**
     * 创建L2PPManager实例
     */
    private final static class SingleHolder {
        private static final L2PPManager INSTANCE = new L2PPManager();
    }

    /**
     * 防止构造函数创建
     */
    private L2PPManager() {
    }

    /**
     * 算路观察者
     */
    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        /**
         * 路线上充电站数据回调    、
         * @param routeL2Data 路线信息
         */
        @Override
        public void onL2DataCallBack(final RouteL2Data routeL2Data) {
            if (routeL2Data == null) {
                Logger.w(TAG, "onL2DataCallBack: routeL2Data null");
                return;
            }
            String json = GsonUtils.toJson(routeL2Data);
            Logger.d(TAG, "send route data: " + json);
//            JsonLog.print("send route data", json);
            JsonLog.saveJsonToCache(json, "l2.json", "l2_route");
            //通过高级辅助驾驶系统管理类 将高德算出来的路线信息发出去
            //DataType.SDRoute 路线数据
            mAdasManager.sendData(DataType.SDRoute, json.getBytes());
        }
    };

    /**
     * TBT数据
     */
    private final L2InfoCallback mL2InfoCallback = new L2InfoCallback() {
        @Override
        public void onSdTbtDataChange(final L2NaviBean l2NaviBean) {
            if (l2NaviBean == null) {
                Logger.w(TAG, "onSdTbtDataChange: l2NaviBean null");
                return;
            }
            String json = GsonUtils.toJson(l2NaviBean);
            Logger.v(TAG, "send tbt data: " + json);
//            JsonLog.print("send tbt data", json);
            JsonLog.saveJsonToCache(json, "l2.json", "l2_tbt");
            mAdasManager.sendData(DataType.SDPeriodShortData, json.getBytes());
        }
    };

    /**
     * Map provider向MFF返回的matching response的接口和数据结构
     */
    private final DataCallback mDataCallback = new DataCallback() {
        @Override
        public void onDataCallback(final DataType dataType, final byte[] bytes) {
            if (dataType == null) {
                Logger.w(TAG, "onDataCallback: dataType null");
                return;
            }
            if (bytes == null) {
                Logger.w(TAG, "onDataCallback: bytes null");
                return;
            }
            Logger.d(TAG, "onDataCallback: dataType = " + dataType);
            if (dataType != DataType.SDMapReserve) {
                return;
            }
            final String jsonString = new String(bytes, StandardCharsets.UTF_8);
            JsonLog.saveJsonToCache(jsonString, "odd.json");
            Logger.d(TAG, "onDataCallback: oddBean = " + jsonString);
            OddBean oddBean = null;
            try {
                oddBean = GsonUtils.fromJson(jsonString, OddBean.class);
            } catch (Exception e) {
                Logger.e(TAG, "onDataCallback: fromJson error", e);
            }
            if (oddBean == null) {
                Logger.e(TAG, "onDataCallback: oddBean null");
                return;
            }
            ArrayList<LayerItemRouteOdd> layerItemRouteOddList = oddBean.toLayerItemRouteOddList();
            long pathId = oddBean.getPathId();
            L2Package.getInstance().updateOddInfo(MapType.MAIN_SCREEN_MAIN_MAP, layerItemRouteOddList, pathId);
        }
    };

    private final PropertyCallback mPropertyCallback = new PropertyCallback() {
        @Override
        public void onPropertyChange(final int propertyId, final byte[] result) {
            if (result == null) {
                Logger.w(TAG, "onPropertyChange: result null");
                return;
            }
            Logger.i(TAG, "onPropertyChange: propertyId = " + propertyId);
            if (propertyId != Properties.ADASWarnings) {
                return;
            }
            ADUProto.ADASWarnings_status status = null;
            try {
                status = ADUProto.ADASWarnings_status.parseFrom(result);
            } catch (InvalidProtocolBufferException e) {
                Logger.e(TAG, "onPropertyChange: parseFrom error", e);
            }
            if (status == null) {
                Logger.w(TAG, "onPropertyChange: status null");
                return;
            }
            checkPropertyId1(status);
            checkPropertyId2(status);
        }
    };

    /**
     * PropertyId处理1
     *
     * @param status ADASWarnings_status
     */
    private void checkPropertyId1(final ADUProto.ADASWarnings_status status) {
//        final ADUProto.aDAS_ServiceUnavailableWarnings adasServiceUnavailableWarnings = status.getADASServiceUnavailableWarnings();
//        switch (adasServiceUnavailableWarnings) {
//            case ADAS_PILOT_SERVICE_LOCKOUT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_pilot_service_lockout));
//                break;
//            case ADAS_AEB_OFF:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_aeb_off));
//                break;
//            case ADAS_RESERVE_SERVICE_UNAVAILABLE_5:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_service_unavailable_5));
//                break;
//            case ADAS_ACC_OFF:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_acc_off));
//                break;
//            case ADAS_LOWSPEED:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lowspeed));
//                break;
//            case ADAS_OVERSPEED:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_overspeed));
//                break;
//            case ADAS_RESERVE_SERVICE_UNAVAILABLE_6:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_service_unavailable_6));
//                break;
//            case ADAS_ARS_TURNING_OFF_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_ars_turning_off_alert));
//                break;
//            case ADAS_ARS_TURNNED_OFF_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_ars_turnned_off_alert));
//                break;
//            case ADAS_CHECK_CAMERA_AND_LRR:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_check_camera_and_lrr));
//                break;
//            case ADAS_GENERAL_SERVICE_UNAVAILABLE_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_general_service_unavailable_alert));
//                break;
//            case ADAS_RESERVE_SERVICE_UNAVAILABLE_4:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_service_unavailable_4));
//                break;
//            default:
//        }
//        final ADUProto.aDAS_FeatureStateWarnings adasFeatureStateWarnings = status.getADASFeatureStateWarnings();
//        switch (adasFeatureStateWarnings) {
//            case ADAS_PILOT_SERVICE_FAIL_ACTIVED_ALERT_AND_CHANGE_SETTINGS_PROMPT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_pilot_service_fail_actived_alert_and_change_settings_prompt));
//                break;
//            case ADAS_FEATURE_STATE_WARNINGS_RESERVE_2:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_feature_state_warnings_reserve_2));
//                break;
//            case ADAS_NP_SERVICE_ACTIVE_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_np_service_active_alert));
//                break;
//            case ADAS_CP_SERVICE_ACTIVE_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_cp_service_active_alert));
//                break;
//            case ADAS_UPGRADE_TO_NP_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_upgrade_to_np_alert));
//                break;
//            case ADAS_DOWNGRADE_TO_CP_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_downgrade_to_cp_alert));
//                break;
//            case ADAS_APPROACHING_PILOT_ODD:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_approaching_pilot_odd));
//                break;
//            default:
//        }
        final ADUProto.aDAS_ServiceEndWarnings adasServiceEndWarnings = status.getADASServiceEndWarnings();
        switch (adasServiceEndWarnings) {
            case ADAS_OVERTIME_STOP_ALERT:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_overtime_stop_alert));
                break;
//            case ADAS_RESERVE_SERVICE_END_1:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_service_end_1));
//                break;
            default:
        }
//        final ADUProto.aDAS_SensorCleanWarnings adasSensorCleanWarnings = status.getADASSensorCleanWarnings();
//        switch (adasSensorCleanWarnings) {
//            case ADAS_CAMERA_CLEAN_SYSTEM_FAULT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_camera_clean_system_fault));
//                break;
//            default:
//        }
    }

    /**
     * PropertyId处理2
     *
     * @param status ADASWarnings_status
     */
    private void checkPropertyId2(final ADUProto.ADASWarnings_status status) {
        final ADUProto.aDAS_GeneralSafetyWarnings adasGeneralSafetyWarnings = status.getADASGeneralSafetyWarnings();
        switch (adasGeneralSafetyWarnings) {
//            case ADAS_RESERVE_GENERAL_SAFETY_5:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_5));
//                break;
//            case ADAS_RESERVE_GENERAL_SAFETY_6:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_6));
//                break;
//            case ADAS_RESERVE_GENERAL_SAFETY_7:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_7));
//                break;
//            case ADAS_RESERVE_GENERAL_SAFETY_13:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_13));
//                break;
            case ADAS_RESERVE_GENERAL_SAFETY_14:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_14));
                break;
            case ADAS_RESERVE_GENERAL_SAFETY_15:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_15));
                break;
            case ADAS_RESERVE_GENERAL_SAFETY_10:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_10));
                break;
            case ADAS_RESERVE_GENERAL_SAFETY_11:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_11));
                break;
//            case ADAS_ALERT_OF_PASSING_CROSS:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_alert_of_passing_cross));
//                break;
            case ADAS_DODGE_TRUCK:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_dodge_truck));
                break;
//            case ADAS_COLLISION_RISK_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_collision_risk_alert));
//                break;
//            case ADAS_PREALERT_OF_PASSING_CROSS:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_prealert_of_passing_cross));
//                break;
//            case ADAS_ATTENTION_TO_PEDESTRIAN:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_attention_to_pedestrian));
//                break;
//            case ADAS_RESERVE_GENERAL_SAFETY_12:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_general_safety_12));
//                break;
            default:
        }
        final ADUProto.aDAS_LaneCancelWarnings adasLaneCancelWarnings = status.getADASLaneCancelWarnings();
        switch (adasLaneCancelWarnings) {
            case ADAS_LANE_CHANGE_TO_LEFT_RIGHTNOW:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_change_to_left_rightnow));
                break;
            case ADAS_LANE_CHANGE_TO_RIGHT_RIGHTNOW:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_change_to_right_rightnow));
                break;
            case ADAS_RESERVE_LANE_CHANGE_4:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_lane_change_4));
                break;
            case ADAS_RESERVE_LANE_CHANGE_5:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_lane_change_5));
                break;
            case ADAS_RESERVE_LANE_CHANGE_6:
                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_lane_change_6));
                break;
//            case ADAS_LANE_CHANGE_CONDITION_NOT_MEET:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_change_condition_not_meet));
//                break;
//            case ADAS_LEFT_TURNING:
//            case ADAS_RIGHT_TURNING:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_turning));
//                break;
//            case ADAS_LANE_CHANGE_ABORTED:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_change_aborted));
//                break;
//            case ADAS_RESERVE_LANE_CHANGE_3:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_reserve_lane_change_3));
//                break;
//            case ADAS_LANE_MERGING_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_merging_alert));
//                break;
//            case ADAS_LANE_DIVERSION_ALERT:
//                L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_adas_lane_diversion_alert));
//                break;
            default:
        }
    }

    /**
     * 车速变化
     */
    private SignalCallback mSignalCallback = new SignalCallback() {
        @Override
        public void onLaneCenteringWarningIndicationRequestIdcmAChanged(final int state) {
            Logger.i(TAG, "onCanSignalChanged: " + state);
            switch (state) {
                case 0xD:
                case 0xB:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_db), true);
                    break;
                case 0x8:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_8), true);
                    break;
//                case 0x32:
//                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_32));
//                    break;
//                case 0xA:
//                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_a));
//                    break;
//                case 0x15:
//                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_15));
//                    break;
                case 0x17:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_17), true);
                    break;
                case 0x13:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_13), true);
                    break;
                case 0x35:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_35), true);
                    break;
                case 0x7:
                    L2NopTts.sendTTS(AppContext.getInstance().getMContext().getString(R.string.str_signal_changed_7), true);
                    break;
                default:
            }
        }
    };

    /**
     * 初始化
     *
     * @param adasManager
     */
    public void init(final AdasManager adasManager) {
        if (CalibrationPackage.getInstance().adasConfigurationType() != 8) {
            Logger.i(TAG, "not GB Arch ADCU configuration");
            return;
        }
        if (mInitialized) {
            Logger.i(TAG, "initialized");
            return;
        }
        mAdasManager = adasManager;
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
        SignalPackage.getInstance().registerObserver(TAG, mSignalCallback);
        // odd接口
        mAdasManager.setDataCallback(mDataCallback);
        // tts接口
        mAdasManager.registerADUPropertyCallback(mPropertyCallback);
        mInitialized = true;
    }

    public void testInit() {
        if (mInitialized) {
            Logger.i(TAG, "initialized");
            return;
        }
        mAdasManager = AdasManager.getInstance(AppContext.getInstance().getMContext());
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
        SignalPackage.getInstance().registerObserver(TAG, mSignalCallback);
        mInitialized = true;
    }

    /**
     * 销毁
     */
    public void uninit() {
        if (!mInitialized) {
            Logger.i(TAG, "not initialized");
            return;
        }
        Logger.i(TAG, "uninit");
        RoutePackage.getInstance().unRegisterRouteObserver(TAG);
        L2Package.getInstance().unregisterCallback(TAG);
        SignalPackage.getInstance().unregisterObserver(TAG);
        mAdasManager.removeDataCallback();
        mAdasManager.unregisterADUPropertyCallback();
        mInitialized = false;
    }
}
