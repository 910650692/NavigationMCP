package com.sgm.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.sgm.navi.scene.databinding.SceneRoutePreferenceViewBinding;
import com.sgm.navi.scene.impl.preference.SceneRoutePreferenceImpl;
import com.sgm.navi.service.define.route.RoutePreferenceID;

import java.util.Hashtable;


public class SceneRoutePreferenceView extends BaseSceneView<SceneRoutePreferenceViewBinding, SceneRoutePreferenceImpl>
        implements SceneRoutePreferenceImpl.IRoutePreferenceChangeListener {
    public static final String TAG = SceneRoutePreferenceView.class.getSimpleName();
    private Hashtable<String, ISceneRoutePreferenceCallBack> mSceneRoutePreferenceCallBackMap;

    public SceneRoutePreferenceView(final Context context) {
        super(context);
    }

    public SceneRoutePreferenceView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRoutePreferenceView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRoutePreferenceViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRoutePreferenceViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRoutePreferenceImpl initSceneImpl() {
        mSceneRoutePreferenceCallBackMap = new Hashtable<>();
        return new SceneRoutePreferenceImpl(TAG, this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRoutePreferenceObserver(final String key, final ISceneRoutePreferenceCallBack callBack) {
        mSceneRoutePreferenceCallBackMap.put(key, callBack);
    }


    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        mScreenViewModel.setOnPreferenceChangeListener("route fragment",this);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
        mScreenViewModel.setDefaultPreference();
        setPreferenceEnable(getNetworkState());
    }

    @Override
    public void onDestroy() {
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mScreenViewModel.unSettingChangeCallback(TAG);
    }
    
    /**
     * 重置偏好
     * */
    public void resetPreference() {
        mScreenViewModel.clearPreference();
        mScreenViewModel.setDefaultPreference();
    }
    /**
     * 偏好转文本
     * @param routePreferenceID 偏好ID
     * @return 文本
     * */
    private String getPreferText(final RoutePreferenceID routePreferenceID) {
        String preferText = "";
        switch (routePreferenceID) {
            case PREFERENCE_RECOMMEND:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_recommend);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion);
                return preferText;
            case PREFERENCE_LESSCHARGE:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_less_charge);
                return preferText;
            case PREFERENCE_NOTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_not_highway);
                return preferText;
            case PREFERENCE_FIRSTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_first_highway);
                return preferText;
            case PREFERENCE_FIRSTMAINROAD:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_first_main_road);
                return preferText;
            case PREFERENCE_FASTESTSPEED:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_fastest_speed);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion_and_less_charge);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion_and_not_highway);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion_and_first_highway);
                return preferText;
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_less_charge_and_not_highway);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY:
                preferText = ResourceUtils.Companion.getInstance()
                        .getString(R.string.route_preference_avoiding_congestion_and_less_charge_not_highway);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion_and_first_main_road);
                return preferText;
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED:
                preferText = ResourceUtils.Companion.getInstance().getString(R.string.route_preference_avoiding_congestion_and_fastest_speed);
                return preferText;
            default:
                return preferText;

        }
    }

    @Override
    public void onPreferenceChange(final RoutePreferenceID routePreference, final boolean isFirstChange) {
        mViewBinding.preferenceRecommend.setSelected(mScreenViewModel.isISRECOMMENDSELECT());
        mViewBinding.preferenceAvoidCongestion.setSelected(mScreenViewModel.isISAVOIDCONGESTIONSELECT());
        mViewBinding.preferenceLessCharge.setSelected(mScreenViewModel.isISLESSCHARGESELECT());
        mViewBinding.preferenceNotHighway.setSelected(mScreenViewModel.isISNOTHIGHWAYSELECT());
        mViewBinding.preferenceFirstHighway.setSelected(mScreenViewModel.isISFIRSTHIGHWAYSELECT());
        mViewBinding.preferenceFirstMainRoad.setSelected(mScreenViewModel.isISFIRSTMAINROADSELECT());
        mViewBinding.preferenceFastestSpeed.setSelected(mScreenViewModel.isISFASTESTSPEEDSELECT());

        for (ISceneRoutePreferenceCallBack callBack : mSceneRoutePreferenceCallBackMap.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            callBack.onRoutePreferenceChange(getPreferText(routePreference), isFirstChange);
        }
    }

    /**
     * 设置CheckBox是否可用
     * @param isEnable 是否可用
     */
    private void setPreferenceEnable(final boolean isEnable) {
        ThreadManager.getInstance().postUi(() -> {
            if (mViewBinding == null) {
                return;
            }
            mViewBinding.preferenceAvoidCongestion.setAlpha(isEnable ? 1.0f : 0.5f);
            mViewBinding.preferenceFirstMainRoad.setAlpha(isEnable? 1.0f : 0.5f);
            mViewBinding.preferenceFastestSpeed.setAlpha(isEnable? 1.0f : 0.5f);
        });
    }

    /**
     * 获取网络状态
     * @return 网络状态
     */
    private boolean getNetworkState() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {
            setPreferenceEnable(true);
        }

        @Override
        public void onNetDisConnect() {
            setPreferenceEnable(false);
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };
}
