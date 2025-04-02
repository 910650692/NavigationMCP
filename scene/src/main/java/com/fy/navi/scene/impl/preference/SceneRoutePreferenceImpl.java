package com.fy.navi.scene.impl.preference;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRoutePreference;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.Hashtable;


/**
 *
 */
public class SceneRoutePreferenceImpl extends BaseSceneModel<BaseSceneView> implements ISceneRoutePreference {

    private static final String TAG = "RoutePreference click: ";
    private static final String RECOMMEND = "route_preference_remmend";
    private static final String AVOIDCONGESTION = "route_preference_avoid_congestion";
    private static final String LESSCHARGE = "route_preference_less_charge";
    private static final String NOTHIGHWAY = "route_preference_not_highway";
    private static final String FIRSTHIGHWAY = "route_preference_first_highway";
    private static final String FIRSTMAINROAD = "route_preference_first_main_road";
    private static final String FASTESTSPEED = "route_preference_fastest_speed";

    public boolean isISRECOMMENDSELECT() {
        return mISRECOMMENDSELECT;
    }
    public boolean isISAVOIDCONGESTIONSELECT() {
        return mISAVOIDCONGESTIONSELECT;
    }
    public boolean isISLESSCHARGESELECT() {
        return mISLESSCHARGESELECT;
    }
    public boolean isISNOTHIGHWAYSELECT() {
        return mISNOTHIGHWAYSELECT;
    }
    public boolean isISFIRSTHIGHWAYSELECT() {
        return mISFIRSTHIGHWAYSELECT;
    }
    public boolean isISFIRSTMAINROADSELECT() {
        return mISFIRSTMAINROADSELECT;
    }
    public boolean isISFASTESTSPEEDSELECT() {
        return mISFASTESTSPEEDSELECT;
    }
    private boolean mISRECOMMENDSELECT = false;
    private boolean mISAVOIDCONGESTIONSELECT = false;
    private boolean mISLESSCHARGESELECT = false;
    private boolean mISNOTHIGHWAYSELECT = false;
    private boolean mISFIRSTHIGHWAYSELECT = false;
    private boolean mISFIRSTMAINROADSELECT = false;
    private boolean mISFASTESTSPEEDSELECT = false;

    private Hashtable<String, IRoutePreferenceChangeListener> mRoutePreferenceChangeListenerMap;

    private  SettingPackage mSettingPackage;
    private RoutePreferenceID mLastRoutePreferenceID;
    private RoutePreferenceID mRoutePreferenceID = RoutePreferenceID.PREFERENCE_RECOMMEND;

    public SceneRoutePreferenceImpl(final BaseSceneView screenView) {
        super(screenView);
        mSettingPackage = SettingPackage.getInstance();
        mRoutePreferenceChangeListenerMap = new Hashtable<>();
    }

    @Override
    public void preferenceRecommend() {
        setRoutePreference(RECOMMEND);
    }

    @Override
    public void preferenceAvoidCongestion() {
        if (!getNetworkStatus()) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.
                    getInstance().getString(R.string.navi_setting_offline_toast));
            return;
        }
        setRoutePreference(AVOIDCONGESTION);
    }

    @Override
    public void preferenceLessCharge() {
        setRoutePreference(LESSCHARGE);
    }

    @Override
    public void preferenceNotHighway() {
        setRoutePreference(NOTHIGHWAY);
    }

    @Override
    public void preferenceFirstHighway() {
        setRoutePreference(FIRSTHIGHWAY);
    }

    @Override
    public void preferenceFirstMainRoad() {
        if (!getNetworkStatus()) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.
                    getInstance().getString(R.string.navi_setting_offline_toast));
            return;
        }
        setRoutePreference(FIRSTMAINROAD);
    }

    @Override
    public void preferenceFastestSpeed() {
        if (!getNetworkStatus()) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.
                    getInstance().getString(R.string.navi_setting_offline_toast));
            return;
        }
        setRoutePreference(FASTESTSPEED);
    }

    /**
     * 获取网络状态
     * @return 网络状态
     */
    public boolean getNetworkStatus() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }
    /**
     * 注册监听
     * @param name 关键字
     * @param listener 回调
     * */
    public void setOnPreferenceChangeListener(final String name, final IRoutePreferenceChangeListener listener) {
        mRoutePreferenceChangeListenerMap.put(name, listener);
    }
    /**
     * 设置偏好
     * @param mode 偏好
     * */
    public void setRoutePreference(final String mode) {
        mRoutePreferenceID = formatPreference(mode);
        if (mRoutePreferenceID == RoutePreferenceID.PREFERENCE_RECOMMEND && mLastRoutePreferenceID == mRoutePreferenceID) {
            Logger.i("the same route perference");
            return;
        }
        mSettingPackage.setRoutePreference(mRoutePreferenceID);
        if (ConvertUtils.isEmpty(mRoutePreferenceChangeListenerMap)) {
            return;
        }
        for (IRoutePreferenceChangeListener listener : mRoutePreferenceChangeListenerMap.values()) {
            listener.onPreferenceChange(mSettingPackage.getRoutePreference(), false);
        }
        mLastRoutePreferenceID = mRoutePreferenceID;
    }
    /**
     * 清除偏好
     * */
    public void clearPreference() {
        mISRECOMMENDSELECT = false;
        mISFIRSTHIGHWAYSELECT = false;
        mISLESSCHARGESELECT = false;
        mISNOTHIGHWAYSELECT = false;
        mISFIRSTMAINROADSELECT = false;
        mISAVOIDCONGESTIONSELECT = false;
        mISFASTESTSPEEDSELECT = false;
    }
    /**
     * 设置默认
     * */
    public void setDefaultPreference() {
        switch (mSettingPackage.getRoutePreference()) {
            case PREFERENCE_RECOMMEND:
                mISRECOMMENDSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                mISAVOIDCONGESTIONSELECT = true;
                break;
            case PREFERENCE_LESSCHARGE:
                mISLESSCHARGESELECT = true;
                break;
            case PREFERENCE_NOTHIGHWAY:
                mISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                mISFIRSTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_FIRSTMAINROAD:
                mISFIRSTMAINROADSELECT = true;
                break;
            case PREFERENCE_FASTESTSPEED:
                mISFASTESTSPEEDSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE:
                mISAVOIDCONGESTIONSELECT = true;
                mISLESSCHARGESELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY:
                mISAVOIDCONGESTIONSELECT = true;
                mISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY:
                mISAVOIDCONGESTIONSELECT = true;
                mISFIRSTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY:
                mISLESSCHARGESELECT = true;
                mISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY:
                mISAVOIDCONGESTIONSELECT = true;
                mISLESSCHARGESELECT = true;
                mISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD:
                mISAVOIDCONGESTIONSELECT = true;
                mISFIRSTMAINROADSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED:
                mISAVOIDCONGESTIONSELECT = true;
                mISFASTESTSPEEDSELECT = true;
                break;
            default:
                break;
        }

        if (ConvertUtils.isEmpty(mRoutePreferenceChangeListenerMap)) {
            return;
        }
        for (IRoutePreferenceChangeListener listener : mRoutePreferenceChangeListenerMap.values()) {
            listener.onPreferenceChange(mSettingPackage.getRoutePreference(), true);
        }
        mLastRoutePreferenceID = mSettingPackage.getRoutePreference();
    }
    /**
     * 格式化偏好
     * @param mode 模式
     * @return 偏好
     * */
    public RoutePreferenceID formatPreference(final String mode){
        switch (mode){
            case RECOMMEND:
                if (!mISRECOMMENDSELECT){
                    mISRECOMMENDSELECT = true;
                    mISAVOIDCONGESTIONSELECT = false;
                    mISLESSCHARGESELECT = false;
                    mISNOTHIGHWAYSELECT = false;
                    mISFIRSTHIGHWAYSELECT = false;
                    mISFIRSTMAINROADSELECT = false;
                    mISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                } else {
                    return RoutePreferenceID.PREFERENCE_RECOMMEND;
                }
            case AVOIDCONGESTION:
                if (mISAVOIDCONGESTIONSELECT) {
                    mISAVOIDCONGESTIONSELECT = false;
                    if (!mISLESSCHARGESELECT && !mISNOTHIGHWAYSELECT && !mISFIRSTHIGHWAYSELECT && !mISFIRSTMAINROADSELECT && !mISFASTESTSPEEDSELECT) {
                        mISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                } else {
                    mISAVOIDCONGESTIONSELECT = true;
                    mISRECOMMENDSELECT = false;
                    return getPreferenceID();
                }
            case LESSCHARGE:
                if (mISLESSCHARGESELECT) {
                    mISLESSCHARGESELECT = false;
                    if (!mISAVOIDCONGESTIONSELECT && !mISNOTHIGHWAYSELECT) {
                        mISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                } else {
                    mISLESSCHARGESELECT = true;
                    mISRECOMMENDSELECT = false;
                    mISFIRSTHIGHWAYSELECT = false;
                    mISFIRSTMAINROADSELECT = false;
                    mISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case NOTHIGHWAY:
                if (mISNOTHIGHWAYSELECT) {
                    mISNOTHIGHWAYSELECT = false;
                    if (!mISAVOIDCONGESTIONSELECT && !mISLESSCHARGESELECT) {
                        mISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                } else {
                    mISNOTHIGHWAYSELECT = true;
                    mISRECOMMENDSELECT = false;
                    mISFIRSTHIGHWAYSELECT = false;
                    mISFIRSTMAINROADSELECT = false;
                    mISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FIRSTHIGHWAY:
                if (mISFIRSTHIGHWAYSELECT) {
                    mISFIRSTHIGHWAYSELECT = false;
                    if (!mISAVOIDCONGESTIONSELECT) {
                        mISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                } else {
                    mISFIRSTHIGHWAYSELECT = true;
                    mISRECOMMENDSELECT = false;
                    mISLESSCHARGESELECT = false;
                    mISNOTHIGHWAYSELECT = false;
                    mISFIRSTMAINROADSELECT = false;
                    mISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FIRSTMAINROAD:
                if (mISFIRSTMAINROADSELECT) {
                    mISFIRSTMAINROADSELECT = false;
                    if (!mISAVOIDCONGESTIONSELECT) {
                        mISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                } else {
                    mISFIRSTMAINROADSELECT = true;
                    mISRECOMMENDSELECT = false;
                    mISLESSCHARGESELECT = false;
                    mISNOTHIGHWAYSELECT = false;
                    mISFIRSTHIGHWAYSELECT = false;
                    mISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FASTESTSPEED:
                return getFaststSpeed();
            default:
                return RoutePreferenceID.PREFERENCE_RECOMMEND;
        }
    }
    /**
     * 最快
     * @return id
     * */
    private RoutePreferenceID getFaststSpeed() {
        if (mISFASTESTSPEEDSELECT) {
            mISFASTESTSPEEDSELECT = false;
            if (!mISAVOIDCONGESTIONSELECT) {
                mISRECOMMENDSELECT = true;
                return getPreferenceID();
            }
            return getPreferenceID();
        } else {
            mISFASTESTSPEEDSELECT = true;
            mISRECOMMENDSELECT = false;
            mISLESSCHARGESELECT = false;
            mISNOTHIGHWAYSELECT = false;
            mISFIRSTHIGHWAYSELECT = false;
            mISFIRSTMAINROADSELECT = false;
            return getPreferenceID();
        }
    }
    /**
     * 获取id
     * @return id
     * */
    private RoutePreferenceID getPreferenceID() {
        if (mISRECOMMENDSELECT) {
            Logger.i(TAG + "推荐");
            return RoutePreferenceID.PREFERENCE_RECOMMEND;
        } else {
            if (mISAVOIDCONGESTIONSELECT && mISLESSCHARGESELECT && mISNOTHIGHWAYSELECT) {
                Logger.i(TAG + "躲避拥堵+少收费+不走高速");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY;
            }

            if (mISAVOIDCONGESTIONSELECT && mISLESSCHARGESELECT) {
                Logger.i(TAG + "躲避拥堵+少收费");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE;
            }

            if (mISAVOIDCONGESTIONSELECT && mISNOTHIGHWAYSELECT) {
                Logger.i(TAG + "躲避拥堵+不走高速");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY;
            }

            if (mISAVOIDCONGESTIONSELECT && mISFIRSTHIGHWAYSELECT) {
                Logger.i(TAG + "躲避拥堵+高速优先");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY;
            }

            if (mISLESSCHARGESELECT && mISNOTHIGHWAYSELECT) {
                Logger.i(TAG + "少收费+不走高速");
                return RoutePreferenceID.PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY;
            }

            if (mISAVOIDCONGESTIONSELECT && mISFIRSTMAINROADSELECT) {
                Logger.i(TAG + "躲避拥堵+大路优先");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD;
            }

            if (mISAVOIDCONGESTIONSELECT && mISFASTESTSPEEDSELECT) {
                Logger.i(TAG + "躲避拥堵+速度最快");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED;
            }

            if (mISAVOIDCONGESTIONSELECT) {
                Logger.i(TAG + "躲避拥堵");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
            }

            if (mISLESSCHARGESELECT) {
                Logger.i(TAG + "少收费");
                return RoutePreferenceID.PREFERENCE_LESSCHARGE;
            }

            if (mISNOTHIGHWAYSELECT) {
                Logger.i(TAG + "不走高速");
                return RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
            }

            if (mISFIRSTHIGHWAYSELECT) {
                Logger.i(TAG + "高速优先");
                return RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
            }

            if (mISFIRSTMAINROADSELECT) {
                Logger.i(TAG + "大路优先");
                return RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
            }

            if (mISFASTESTSPEEDSELECT) {
                Logger.i(TAG + "速度最快");
                return RoutePreferenceID.PREFERENCE_FASTESTSPEED;
            }
            Logger.i(TAG + "异常缺失");
        }
        return RoutePreferenceID.PREFERENCE_RECOMMEND;
    }
    public interface IRoutePreferenceChangeListener {
        /**
         * Scene 回调
         * @param routePreference 偏好id
         * @param isFirstChange 首次改变
         * */
        void onPreferenceChange(RoutePreferenceID routePreference, boolean isFirstChange);
    }

    @Override
    public void closeScene() {
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneCloseState,
                NaviSceneId.NAVI_SCENE_PREFERENCE);
    }
}
