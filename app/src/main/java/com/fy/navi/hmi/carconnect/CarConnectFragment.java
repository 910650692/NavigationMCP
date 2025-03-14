package com.fy.navi.hmi.carconnect;

import android.os.Bundle;
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

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class CarConnectFragment extends BaseFragment<FragmentCarConnectBinding, CarConnectViewModel> {

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
        mBinding.cbCarConnectLoginFunction.setOnCheckedChangeListener(onCheckedChangeListener);
    }

    private void initView() {
        String isSendDestinationLastMile = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE);
        if(isSendDestinationLastMile != null && !isSendDestinationLastMile.isEmpty()){
            mBinding.cbCarConnectLoginFunction.setChecked(SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE).equals("1"));
        } else {
            mBinding.cbCarConnectLoginFunction.setChecked(true);
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, "1");
        }

    }

    public void clearUserInfo() {
        mBinding.tvUserName.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_user_name));
        mBinding.tvConnectTip.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_not_connected));
        mBinding.ivUserIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.mipmap.default_user_icon));
    }

    private void initData(){
        Bundle bundle = getArguments();
        if(bundle != null){
            mBinding.tvUserName.setText(bundle.getString("userName"));
            mBinding.tvConnectTip.setText(ResourceUtils.Companion.getInstance().getText(R.string.car_connect_login_already_connected));
            ViewAdapterKt.loadImageUrl(mBinding.ivUserIcon, Objects.requireNonNull(bundle.getString("userIcon")), R.mipmap.default_user_icon, R.mipmap.default_user_icon);
        } else {
            Logger.e("bundle is null");
        }
    }

    private final CompoundButton.OnCheckedChangeListener onCheckedChangeListener = new CompoundButton.OnCheckedChangeListener() {
        @Override
        public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            Logger.e("onCheckedChanged: " + isChecked);
            if(isChecked){
                SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, "1");
            } else {
                SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE, "0");
            }
        }
    };
}
