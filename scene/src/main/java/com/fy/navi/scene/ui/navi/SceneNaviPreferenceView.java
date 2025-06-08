package com.fy.navi.scene.ui.navi;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.databinding.SceneNaviPreferenceViewBinding;
import com.fy.navi.scene.impl.preference.SceneRoutePreferenceImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
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

    public static final String TAG = SceneNaviPreferenceView.class.getSimpleName();

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
    protected SceneNaviPreferenceViewBinding createViewBinding(final LayoutInflater inflater,
                                                               final ViewGroup viewGroup) {
        return SceneNaviPreferenceViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRoutePreferenceImpl initSceneImpl() {
        mSceneRoutePreferenceCallBackMap = new Hashtable<>();
        return new SceneRoutePreferenceImpl(TAG, this);
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
        boolean isNetAvailable = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().
                checkNetwork());
        Logger.i(TAG, "setInitVariable isNetAvailable = " + isNetAvailable);
        disAbleButton(isNetAvailable);
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

    @SuppressLint("SetTextI18n")
    @Override
    public void onPreferenceChange(final RoutePreferenceID routePreference,
                                   final boolean isFirstChange) {
        Logger.i(TAG, "onPreferenceChange = " + routePreference);
        // 第一个选择的路线偏好
        String firstCommendText = "";
        // 第二个选择的路线偏好
        String secondCommendText = "";
        if (mScreenViewModel == null) {
            return;
        }
        boolean isReCommendSelect = mScreenViewModel.isISRECOMMENDSELECT();
        if (isReCommendSelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().
                    getString(R.string.route_preference_recommend);
        }
        mViewBinding.preferenceRecommend.setSelected(isReCommendSelect);
        boolean isIsAvoidCongestionSelect = mScreenViewModel.isISAVOIDCONGESTIONSELECT();
        mViewBinding.preferenceAvoidCongestion.setSelected(isIsAvoidCongestionSelect);
        if (isIsAvoidCongestionSelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_avoiding_congestion);
        } else if (isIsAvoidCongestionSelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_avoiding_congestion);
        }
        boolean isIsFirstMainRoadSelect = mScreenViewModel.isISFIRSTMAINROADSELECT();
        mViewBinding.preferenceFirstMainRoad.setSelected(isIsFirstMainRoadSelect);
        if (isIsFirstMainRoadSelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_first_main_road);
        } else if (isIsFirstMainRoadSelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_first_main_road);
        }
        boolean isIsFirstHighWaySelect = mScreenViewModel.isISFIRSTHIGHWAYSELECT();
        mViewBinding.preferenceFirstHighway.setSelected(isIsFirstHighWaySelect);
        if (isIsFirstHighWaySelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_first_highway);
        } else if (isIsFirstHighWaySelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_first_highway);
        }
        boolean isIsNotHighWaySelect = mScreenViewModel.isISNOTHIGHWAYSELECT();
        mViewBinding.preferenceNotHighway.setSelected(isIsNotHighWaySelect);
        if (isIsNotHighWaySelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_not_highway);
        } else if (isIsNotHighWaySelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_not_highway);
        }
        boolean isIsLessChagresSelect = mScreenViewModel.isISLESSCHARGESELECT();
        mViewBinding.preferenceLessCharge.setSelected(isIsLessChagresSelect);
        if (isIsLessChagresSelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_less_charge);
        } else if (isIsLessChagresSelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_less_charge);
        }
        boolean isIsFastestSpeedSelect = mScreenViewModel.isISFASTESTSPEEDSELECT();
        mViewBinding.preferenceFastestSpeed.setSelected(isIsFastestSpeedSelect);
        if (isIsFastestSpeedSelect && !firstCommendText.isEmpty()) {
            secondCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_fastest_speed);
        } else if (isIsFastestSpeedSelect) {
            firstCommendText = ResourceUtils.Companion.getInstance().getString(
                    R.string.route_preference_fastest_speed);
        }
        for (ISceneRoutePreferenceCallBack callBack : mSceneRoutePreferenceCallBackMap.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            mScreenViewModel.closeScene();
            callBack.onRoutePreferenceChange(getPreferText(routePreference), isFirstChange);
        }
        if (!firstCommendText.isEmpty() && !secondCommendText.isEmpty()) {
            assert mViewBinding.stvRoutePreferenceSelected != null;
            mViewBinding.stvRoutePreferenceSelected.setText(
                    firstCommendText + "+" + secondCommendText);
            return;
        }
        if (!firstCommendText.isEmpty()) {
            assert mViewBinding.stvRoutePreferenceSelected != null;
            mViewBinding.stvRoutePreferenceSelected.setText(firstCommendText);
        }
    }

    public void updateRoutePreference(final RoutePreferenceID routePreference) {
        if (mScreenViewModel != null) {
            mScreenViewModel.clearPreference();
            mScreenViewModel.setDefaultPreference();
            onPreferenceChange(routePreference, true);
        }
    }

    @Override
    public void onDestroy() {
        Logger.i(TAG, "onDestroy");
        mScreenViewModel.unSettingChangeCallback(TAG);
    }

    /**
     * @param isConnected isConnected
     */
    public void onNetStatusChange(boolean isConnected) {
        disAbleButton(isConnected);
    }

    private void disAbleButton(boolean isConnected) {
        Logger.i(TAG, "disAbleButton isConnected:" + isConnected);
        mViewBinding.preferenceAvoidCongestion.setAlpha(isConnected ? 1.0f : 0.5f);
        mViewBinding.preferenceFirstMainRoad.setAlpha(isConnected ? 1.0f : 0.5f);
        mViewBinding.preferenceFastestSpeed.setAlpha(isConnected ? 1.0f : 0.5f);
    }

    public void closeScene() {
        Logger.i(TAG, "closeScene");
        // TODO: 2025/3/29 当前Scene怎么能控制其他Scene呢？
/*        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneCloseState,
                NaviSceneId.NAVI_SCENE_PREFERENCE);
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneShowState,
                NaviSceneId.NAVI_SCENE_CONTROL);
        mISceneCallback.showControlDetails();*/
    }
}
