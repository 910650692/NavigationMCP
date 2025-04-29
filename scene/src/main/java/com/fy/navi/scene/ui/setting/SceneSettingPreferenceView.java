package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.NetWorkUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.SceneSettingPreferenceBinding;
import com.fy.navi.scene.impl.preference.SceneRoutePreferenceImpl;
import com.fy.navi.service.define.route.RoutePreferenceID;

public class SceneSettingPreferenceView extends BaseSceneView<SceneSettingPreferenceBinding, SceneRoutePreferenceImpl>
        implements SceneRoutePreferenceImpl.IRoutePreferenceChangeListener{
    public static final String TAG = SceneSettingPreferenceView.class.getSimpleName();

    public SceneSettingPreferenceView(@NonNull final Context context) {
        super(context);
    }

    public SceneSettingPreferenceView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneSettingPreferenceView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneSettingPreferenceBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneSettingPreferenceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRoutePreferenceImpl initSceneImpl() {
        return new SceneRoutePreferenceImpl(TAG,this);
    }

    @Override
    protected void setInitVariableId() {
        if (mViewBinding != null) {
            mViewBinding.setSceneSettingPreferenceView(mScreenViewModel);
        }
    }

    @Override
    protected void initObserver() {
        mScreenViewModel.setOnPreferenceChangeListener("setting fragment",this);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
        setPreferenceEnable(getNetworkState());
        mScreenViewModel.setDefaultPreference();
    }

    @Override
    public void onDestroy() {
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mScreenViewModel.unSettingChangeCallback(TAG);
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
