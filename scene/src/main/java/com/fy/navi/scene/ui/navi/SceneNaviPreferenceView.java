package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.databinding.SceneNaviPreferenceViewBinding;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.route.SceneRoutePreferenceImpl;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.route.RoutePreferenceID;

import java.util.Hashtable;

/**
 * 路线偏好
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviPreferenceView extends NaviSceneBase
        <SceneNaviPreferenceViewBinding, SceneRoutePreferenceImpl> implements
        SceneRoutePreferenceImpl.IRoutePreferenceChangeListener {

    private ISceneCallback mISceneCallback;
    private Hashtable<String, ISceneRoutePreferenceCallBack> mSceneRoutePreferenceCallBackMap;

    public SceneNaviPreferenceView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviPreferenceView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviPreferenceView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_PREFERENCE;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_PREFERENCE, this);
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_PREFERENCE, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_PREFERENCE, true);
        }
    }

    @Override
    protected SceneNaviPreferenceViewBinding createViewBinding(final LayoutInflater inflater,
                                                               final ViewGroup viewGroup) {
        return SceneNaviPreferenceViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRoutePreferenceImpl initSceneImpl() {
        mSceneRoutePreferenceCallBackMap = new Hashtable<>();
        return new SceneRoutePreferenceImpl(this);
    }

    /**
     * @param key ket
     * @param callBack 回调
     */
    public void registerRoutePreferenceObserver(final String key,
                                                final ISceneRoutePreferenceCallBack callBack) {
        mSceneRoutePreferenceCallBackMap.put(key, callBack);
    }


    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        if (mScreenViewModel != null) {
            mScreenViewModel.setOnPreferenceChangeListener("route fragment", this);
            mScreenViewModel.setDefaultPreference();
        }
    }

    /**
     * @param routePreferenceID preferenceId
     * @return 推荐文言
     */
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
                preferText = ResourceUtils.Companion.getInstance().
                        getString(
                                R.string.route_preference_avoiding_congestion_and_less_charge_not_highway);
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
    public void onPreferenceChange(final RoutePreferenceID routePreference,
                                   final boolean isFirstChange) {
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
}
