package com.fy.navi.hmi.account;

import android.text.Editable;
import android.text.TextWatcher;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPhoneNumberLoginBinding;
import com.fy.navi.ui.base.BaseFragment;

public class AccountPhoneNumberLoginFragment extends BaseFragment<FragmentPhoneNumberLoginBinding, AccountPhoneNumberLoginViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_phone_number_login;
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

    }

    private void initView() {
        mBinding.phoneNumberEditText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.setPhoneNumber(s.toString());
            }
        });

        mBinding.verificationCodeEditText.addTextChangedListener(new TextWatcher() {

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.setVerificationCode(s.toString());
            }
        });
    }
}
