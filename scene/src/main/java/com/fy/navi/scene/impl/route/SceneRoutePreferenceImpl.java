package com.fy.navi.scene.impl.route;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRoutePreference;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRoutePreferenceImpl extends BaseSceneModel<BaseSceneView> implements ISceneRoutePreference {

    protected final String RECOMMEND = "route_preference_remmend";
    protected final String AVOIDCONGESTION = "route_preference_avoid_congestion";
    protected final String LESSCHARGE = "route_preference_less_charge";
    protected final String NOTHIGHWAY = "route_preference_not_highway";
    protected final String FIRSTHIGHWAY = "route_preference_first_highway";
    protected final String FIRSTMAINROAD = "route_preference_first_main_road";
    protected final String FASTESTSPEED = "route_preference_fastest_speed";

    public boolean ISRECOMMENDSELECT = false;
    public boolean ISAVOIDCONGESTIONSELECT = false;
    public boolean ISLESSCHARGESELECT = false;
    public boolean ISNOTHIGHWAYSELECT = false;
    public boolean ISFIRSTHIGHWAYSELECT = false;
    public boolean ISFIRSTMAINROADSELECT = false;
    public boolean ISFASTESTSPEEDSELECT = false;

    private Hashtable<String, IRoutePreferenceChangeListener> routePreferenceChangeListenerMap;

    private  SettingPackage mSettingPackage;
    private RoutePreferenceID lastRoutePreferenceID;
    private RoutePreferenceID routePreferenceID = RoutePreferenceID.PREFERENCE_RECOMMEND;

    public SceneRoutePreferenceImpl(BaseSceneView mScreenView) {
        super(mScreenView);
        mSettingPackage = SettingPackage.getInstance();
        routePreferenceChangeListenerMap = new Hashtable<>();
    }

    @Override
    public void preferenceRecommend() {
        setRoutePreference(RECOMMEND);
    }

    @Override
    public void preferenceAvoidCongestion() {
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
        setRoutePreference(FIRSTMAINROAD);
    }

    @Override
    public void preferenceFastestSpeed() {
        setRoutePreference(FASTESTSPEED);
    }

    public void setOnPreferenceChangeListener(String name, IRoutePreferenceChangeListener listener) {
        routePreferenceChangeListenerMap.put(name, listener);
    }

    public void setRoutePreference(String mode) {
        routePreferenceID = formatPreference(mode);
        if (routePreferenceID == RoutePreferenceID.PREFERENCE_RECOMMEND && lastRoutePreferenceID == routePreferenceID) {
            Logger.i("the same route perference");
            return;
        }
        mSettingPackage.setRoutePreference(routePreferenceID);
        if (ConvertUtils.isEmpty(routePreferenceChangeListenerMap)) return;
        for (IRoutePreferenceChangeListener listener : routePreferenceChangeListenerMap.values()) {
            listener.onPreferenceChange(mSettingPackage.getRoutePreference(), false);
        }
        lastRoutePreferenceID = routePreferenceID;
    }

    public void clearPreference() {
        ISRECOMMENDSELECT = false;
        ISFIRSTHIGHWAYSELECT = false;
        ISLESSCHARGESELECT = false;
        ISNOTHIGHWAYSELECT = false;
        ISFIRSTMAINROADSELECT = false;
        ISAVOIDCONGESTIONSELECT = false;
        ISFASTESTSPEEDSELECT = false;
    }

    public void setDefaultPreference() {
        switch (mSettingPackage.getRoutePreference()) {
            case PREFERENCE_RECOMMEND:
                ISRECOMMENDSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION:
                ISAVOIDCONGESTIONSELECT = true;
                break;
            case PREFERENCE_LESSCHARGE:
                ISLESSCHARGESELECT = true;
                break;
            case PREFERENCE_NOTHIGHWAY:
                ISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_FIRSTHIGHWAY:
                ISFIRSTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_FIRSTMAINROAD:
                ISFIRSTMAINROADSELECT = true;
                break;
            case PREFERENCE_FASTESTSPEED:
                ISFASTESTSPEEDSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE:
                ISAVOIDCONGESTIONSELECT = true;
                ISLESSCHARGESELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY:
                ISAVOIDCONGESTIONSELECT = true;
                ISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY:
                ISAVOIDCONGESTIONSELECT = true;
                ISFIRSTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY:
                ISLESSCHARGESELECT = true;
                ISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY:
                ISAVOIDCONGESTIONSELECT = true;
                ISLESSCHARGESELECT = true;
                ISNOTHIGHWAYSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD:
                ISAVOIDCONGESTIONSELECT = true;
                ISFIRSTMAINROADSELECT = true;
                break;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED:
                ISAVOIDCONGESTIONSELECT = true;
                ISFASTESTSPEEDSELECT = true;
                break;
        }

        if (ConvertUtils.isEmpty(routePreferenceChangeListenerMap)) return;
        for (IRoutePreferenceChangeListener listener : routePreferenceChangeListenerMap.values()) {
            listener.onPreferenceChange(mSettingPackage.getRoutePreference(), true);
        }
        lastRoutePreferenceID = mSettingPackage.getRoutePreference();
    }

    public RoutePreferenceID formatPreference(String mode){
        switch (mode){
            case RECOMMEND:
                if (!ISRECOMMENDSELECT){
                    ISRECOMMENDSELECT = true;
                    ISAVOIDCONGESTIONSELECT = false;
                    ISLESSCHARGESELECT = false;
                    ISNOTHIGHWAYSELECT = false;
                    ISFIRSTHIGHWAYSELECT = false;
                    ISFIRSTMAINROADSELECT = false;
                    ISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
                else
                {
                    return RoutePreferenceID.PREFERENCE_RECOMMEND;
                }
            case AVOIDCONGESTION:
                if (ISAVOIDCONGESTIONSELECT) {
                    ISAVOIDCONGESTIONSELECT = false;
                    if (!ISLESSCHARGESELECT && !ISNOTHIGHWAYSELECT && !ISFIRSTHIGHWAYSELECT && !ISFIRSTMAINROADSELECT && !ISFASTESTSPEEDSELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISAVOIDCONGESTIONSELECT = true;
                    ISRECOMMENDSELECT = false;
                    return getPreferenceID();
                }
            case LESSCHARGE:
                if (ISLESSCHARGESELECT) {
                    ISLESSCHARGESELECT = false;
                    if (!ISAVOIDCONGESTIONSELECT && !ISNOTHIGHWAYSELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISLESSCHARGESELECT = true;
                    ISRECOMMENDSELECT = false;
                    ISFIRSTHIGHWAYSELECT = false;
                    ISFIRSTMAINROADSELECT = false;
                    ISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case NOTHIGHWAY:
                if (ISNOTHIGHWAYSELECT) {
                    ISNOTHIGHWAYSELECT = false;
                    if (!ISAVOIDCONGESTIONSELECT && !ISLESSCHARGESELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISNOTHIGHWAYSELECT = true;
                    ISRECOMMENDSELECT = false;
                    ISFIRSTHIGHWAYSELECT = false;
                    ISFIRSTMAINROADSELECT = false;
                    ISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FIRSTHIGHWAY:
                if (ISFIRSTHIGHWAYSELECT) {
                    ISFIRSTHIGHWAYSELECT = false;
                    if (!ISAVOIDCONGESTIONSELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISFIRSTHIGHWAYSELECT = true;
                    ISRECOMMENDSELECT = false;
                    ISLESSCHARGESELECT = false;
                    ISNOTHIGHWAYSELECT = false;
                    ISFIRSTMAINROADSELECT = false;
                    ISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FIRSTMAINROAD:
                if (ISFIRSTMAINROADSELECT) {
                    ISFIRSTMAINROADSELECT = false;
                    if (!ISAVOIDCONGESTIONSELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISFIRSTMAINROADSELECT = true;
                    ISRECOMMENDSELECT = false;
                    ISLESSCHARGESELECT = false;
                    ISNOTHIGHWAYSELECT = false;
                    ISFIRSTHIGHWAYSELECT = false;
                    ISFASTESTSPEEDSELECT = false;
                    return getPreferenceID();
                }
            case FASTESTSPEED:
                if (ISFASTESTSPEEDSELECT) {
                    ISFASTESTSPEEDSELECT = false;
                    if (!ISAVOIDCONGESTIONSELECT) {
                        ISRECOMMENDSELECT = true;
                        return getPreferenceID();
                    }
                    return getPreferenceID();
                }
                else
                {
                    ISFASTESTSPEEDSELECT = true;
                    ISRECOMMENDSELECT = false;
                    ISLESSCHARGESELECT = false;
                    ISNOTHIGHWAYSELECT = false;
                    ISFIRSTHIGHWAYSELECT = false;
                    ISFIRSTMAINROADSELECT = false;
                    return getPreferenceID();
                }
            default:
                return RoutePreferenceID.PREFERENCE_RECOMMEND;
        }
    }

    private RoutePreferenceID getPreferenceID() {
        if (ISRECOMMENDSELECT) {
            Logger.i("RoutePreference click: " + "推荐");
            return RoutePreferenceID.PREFERENCE_RECOMMEND;
        }
        else
        {
            if (ISAVOIDCONGESTIONSELECT && ISLESSCHARGESELECT && ISNOTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+少收费+不走高速");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY;
            }

            if (ISAVOIDCONGESTIONSELECT && ISLESSCHARGESELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+少收费");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE;
            }

            if (ISAVOIDCONGESTIONSELECT && ISNOTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+不走高速");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY;
            }

            if (ISAVOIDCONGESTIONSELECT && ISFIRSTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+高速优先");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY;
            }

            if (ISLESSCHARGESELECT && ISNOTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "少收费+不走高速");
                return RoutePreferenceID.PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY;
            }

            if (ISAVOIDCONGESTIONSELECT && ISFIRSTMAINROADSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+大路优先");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD;
            }

            if (ISAVOIDCONGESTIONSELECT && ISFASTESTSPEEDSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵+速度最快");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED;
            }

            if (ISAVOIDCONGESTIONSELECT) {
                Logger.i("RoutePreference click: " + "躲避拥堵");
                return RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
            }

            if (ISLESSCHARGESELECT) {
                Logger.i("RoutePreference click: " + "少收费");
                return RoutePreferenceID.PREFERENCE_LESSCHARGE;
            }

            if (ISNOTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "不走高速");
                return RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
            }

            if (ISFIRSTHIGHWAYSELECT) {
                Logger.i("RoutePreference click: " + "高速优先");
                return RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
            }

            if (ISFIRSTMAINROADSELECT) {
                Logger.i("RoutePreference click: " + "大路优先");
                return RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
            }

            if (ISFASTESTSPEEDSELECT) {
                Logger.i("RoutePreference click: " + "速度最快");
                return RoutePreferenceID.PREFERENCE_FASTESTSPEED;
            }
            Logger.i("RoutePreference click: " + "异常缺失");
        }
        return RoutePreferenceID.PREFERENCE_RECOMMEND;
    }

    public interface IRoutePreferenceChangeListener {
        void onPreferenceChange(RoutePreferenceID routePreference, boolean isFirstChange);
    }

}
