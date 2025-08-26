package com.sgm.navi.hmi.setting.others.protocol;

import android.view.View;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentProtocolCenterBinding;
import com.sgm.navi.ui.base.BaseFragment;

public class SettingOthersProtocolCenterFragment extends BaseFragment<FragmentProtocolCenterBinding, SettingOthersProtocolCenterViewModel> {

    public enum ProtocolCenterType {
        PROTOCOL_TERM,
        PROTOCOL_PRIVACY,
        PROTOCOL_PROTOCOL
    }

    @Override
    public int onLayoutId() {
        return R.layout.fragment_protocol_center;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.llProtocolCenter.setVisibility(View.VISIBLE);
        mBinding.protocolCenterPolicy.setVisibility(View.GONE);
    }

    @Override
    public void onInitData() {

    }

    /**
     * 展示协议中心
     * @param type 协议中心类型
     */
    public void showProtocolCenter(final ProtocolCenterType type) {
        mBinding.llProtocolCenter.setVisibility(View.GONE);

        switch (type){
            case PROTOCOL_TERM:
                mBinding.protocolCenterTitle.setText(R.string.reminder_page_service_title);
                mBinding.protocolCenterContent.setText(R.string.reminder_page_service_content);
                break;
            case PROTOCOL_PRIVACY:
                mBinding.protocolCenterTitle.setText(R.string.reminder_page_privacy_title);
                mBinding.protocolCenterContent.setText(R.string.reminder_page_privacy_content);
                break;
            default:
                break;
        }

        mBinding.protocolCenterPolicy.setVisibility(View.VISIBLE);
        mBinding.protocolCenterContainer.scrollTo(0, 0);
    }

    /**
     * 关闭协议中心
     */
    public void closeProtocolCenter() {
        mBinding.llProtocolCenter.setVisibility(View.VISIBLE);
        mBinding.protocolCenterPolicy.setVisibility(View.GONE);
    }

}
