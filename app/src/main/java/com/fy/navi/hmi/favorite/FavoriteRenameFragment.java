package com.fy.navi.hmi.favorite;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.View;
import android.view.WindowManager;
import android.widget.TextView;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentFavoriteRenameBinding;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.base.BaseFragment;

public class FavoriteRenameFragment extends BaseFragment<FragmentFavoriteRenameBinding, FavoriteRenameViewModel> {

    private String mName = "";
    @Override
    public int onLayoutId() {
        return R.layout.fragment_favorite_rename;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.settingFavoriteRename.setOnFocusChangeListener((v, hasFocus) -> {
            if (hasFocus) {
                showSoftKeyboard();
            }
        });
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if (bundle != null) {
            final PoiInfoEntity poiInfoEntity = bundle.getParcelable("rename");
            if (poiInfoEntity != null) {
                String customName = "";
                if (poiInfoEntity.getFavoriteInfo() != null) {
                    customName = poiInfoEntity.getFavoriteInfo().getCustom_name();
                    mName = poiInfoEntity.getName();
                }
                setEditTextContent(TextUtils.isEmpty(customName) ? poiInfoEntity.getName() : customName);
            }
        }
    }

    /**
     * setEditTextContent
     * @param content
     */
    public void setEditTextContent(String content) {
        mBinding.settingFavoriteRename.requestFocus();
        mBinding.settingFavoriteRename.setText(content);
        mBinding.settingFavoriteRename.setSelection(content.length());
    }

    /**
     * clearEditText
     */
    public void clearEditText() {
        mBinding.settingFavoriteRename.setText("");
    }

    /**
     * renameFinished
     */
    public void renameFinished() {
        String newName = mBinding.settingFavoriteRename.getText().toString().trim();
        SettingUpdateObservable.getInstance().onUpdateRename(TextUtils.isEmpty(newName) ? mName : newName);
    }

    /**
     * 显示系统键盘
     */
    private void showSoftKeyboard() {
        if (getActivity() == null || getActivity().getWindow() == null) {
            return;
        }
        getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
    }
}
