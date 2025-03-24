package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.CompoundButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneSettingPreferenceBinding;
import com.fy.navi.scene.impl.route.SceneRoutePreferenceImpl;
import com.fy.navi.service.define.route.RoutePreferenceID;

public class SceneSettingPreferenceView extends BaseSceneView<SceneSettingPreferenceBinding, SceneRoutePreferenceImpl>
        implements SceneRoutePreferenceImpl.IRoutePreferenceChangeListener{
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
        return new SceneRoutePreferenceImpl(this);
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
        mViewBinding.preferenceRecommend.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceLessCharge.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceNotHighway.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceFirstHighway.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceAvoidCongestion.setOnCheckedChangeListener((buttonView, isChecked)
                -> ThreadManager.getInstance().postUi(() -> {
            if(!getNetworkState()) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.navi_setting_offline_toast));
            } else {
                updateCheckBoxTextColor(buttonView, isChecked);
                setBackgroundColor(buttonView, isChecked);
            }
        }));

        mViewBinding.preferenceFirstMainRoad.setOnCheckedChangeListener((buttonView, isChecked)
                -> ThreadManager.getInstance().postUi(() -> {
            if(!getNetworkState()) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.navi_setting_offline_toast));
            } else {
                updateCheckBoxTextColor(buttonView, isChecked);
                setBackgroundColor(buttonView, isChecked);
            }
        }));
        mViewBinding.preferenceFastestSpeed.setOnCheckedChangeListener((buttonView, isChecked)
                -> ThreadManager.getInstance().postUi(() -> {
            if(!getNetworkState()) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.navi_setting_offline_toast));
            } else {
                updateCheckBoxTextColor(buttonView, isChecked);
                setBackgroundColor(buttonView, isChecked);
            }
        }));

        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
        mScreenViewModel.setDefaultPreference();
        setPreferenceEnable(getNetworkState());
        updateDisableButton();
    }

    /**
     * 更新CheckBox文本颜色
     * @param compoundButton CheckBox
     * @param isSelected 是否选中
     */
    public void updateCheckBoxTextColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
        } else {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
        }
    }

    /**
     * 更新背景色
     * @param compoundButton CheckBox
     * @param isSelected 是否选中
     */
    private void setBackgroundColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_preference_select));
        } else {
            compoundButton.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_preference_normal));
        }
    }

    /**
     * 更新Disable CheckBox文本背景颜色
     */
    private void updateDisableButton() {
        updateCheckBoxTextColor(mViewBinding.preferenceAvoidCongestion, mScreenViewModel.isISAVOIDCONGESTIONSELECT());
        updateCheckBoxTextColor(mViewBinding.preferenceFirstMainRoad, mScreenViewModel.isISFIRSTMAINROADSELECT());
        updateCheckBoxTextColor(mViewBinding.preferenceFastestSpeed, mScreenViewModel.isISFASTESTSPEEDSELECT());
        setBackgroundColor(mViewBinding.preferenceAvoidCongestion, mScreenViewModel.isISAVOIDCONGESTIONSELECT());
        setBackgroundColor(mViewBinding.preferenceFirstMainRoad, mScreenViewModel.isISFIRSTMAINROADSELECT());
        setBackgroundColor(mViewBinding.preferenceFastestSpeed, mScreenViewModel.isISFASTESTSPEEDSELECT());
    }

    @Override
    public void onPreferenceChange(final RoutePreferenceID routePreference, final boolean isFirstChange) {
        mViewBinding.preferenceRecommend.setChecked(mScreenViewModel.isISRECOMMENDSELECT());
        mViewBinding.preferenceAvoidCongestion.setChecked(mScreenViewModel.isISAVOIDCONGESTIONSELECT());
        mViewBinding.preferenceLessCharge.setChecked(mScreenViewModel.isISLESSCHARGESELECT());
        mViewBinding.preferenceNotHighway.setChecked(mScreenViewModel.isISNOTHIGHWAYSELECT());
        mViewBinding.preferenceFirstHighway.setChecked(mScreenViewModel.isISFIRSTHIGHWAYSELECT());
        mViewBinding.preferenceFirstMainRoad.setChecked(mScreenViewModel.isISFIRSTMAINROADSELECT());
        mViewBinding.preferenceFastestSpeed.setChecked(mScreenViewModel.isISFASTESTSPEEDSELECT());
        updateDisableButton();
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
            updateDisableButton();
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
