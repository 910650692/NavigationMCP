package com.fy.navi.hmi.carconnect;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.CompoundButton;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentCarConnectBinding;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Objects;

public class CarConnectFragment extends BaseFragment<FragmentCarConnectBinding, CarConnectViewModel> {

    public static final String DESTINATION_SEND = "1";
    private static final String DESTINATION_UNSENT = "0";

    @Override
    public int onLayoutId() {
        return R.layout.fragment_car_connect;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initView();
    }

    @Override
    public void onInitData() {
        initData();
    }

    @Override
    public void onInitObserver() {
        mBinding.cbCarConnectLoginFunction.setOnCheckedChangeListener(mOnCheckedChangeListener);
    }

    /**
     * init view
     */
    private void initView() {
        final String isSendDestinationLastMile = SettingManager.getInstance().getValueByKey(
            SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE);
        if(!TextUtils.isEmpty(isSendDestinationLastMile)){
            mBinding.cbCarConnectLoginFunction.setChecked(SettingManager.getInstance().getValueByKey(
                SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE).equals(DESTINATION_SEND));
        } else {
            mBinding.cbCarConnectLoginFunction.setChecked(true);
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, DESTINATION_SEND);
        }

    }

    /**
     * clear user info
     */
    public void clearUserInfo() {
        mBinding.tvUserName.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_user_name));
        mBinding.tvConnectTip.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_not_connected));
        mBinding.ivUserIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.mipmap.default_user_icon));
    }

    /**
     * init data
     */
    private void initData(){
        final Bundle bundle = getArguments();
        if(bundle != null){
            mBinding.tvUserName.setText(bundle.getString("userName"));
            mBinding.tvConnectTip.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_already_connected));
            ViewAdapterKt.loadImageUrl(mBinding.ivUserIcon, Objects.requireNonNull(bundle.getString("userIcon")),
                R.mipmap.default_user_icon, R.mipmap.default_user_icon);
        } else {
            Logger.e("bundle is null");
        }
    }

    private final CompoundButton.OnCheckedChangeListener mOnCheckedChangeListener = (buttonView, isChecked) -> {
        Logger.e("onCheckedChanged: " + isChecked);
        setLastOneMile(isChecked);
    };

    /**
     * 设置最后一公里选中状态
     * @param isChecked 选中状态
     */
    public void setLastOneMile(final boolean isChecked) {
        if(isChecked){
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, DESTINATION_SEND);
        } else {
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, DESTINATION_UNSENT);
        }
    }

    /**
     * 设置最后一公里选中状态
     * @param isChecked 选中状态
     */
    public void setLastOneMileChecked(final boolean isChecked) {
        mBinding.cbCarConnectLoginFunction.setChecked(isChecked);
    }
}
