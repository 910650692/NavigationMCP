package com.sgm.navi.hmi.traffic;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.List;

/**
 * Author: QiuYaWei
 * Date: 2025/3/1
 * Description: [在这里描述文件功能]
 */
public class PicPageAdapter extends FragmentStateAdapter {
    private List<String> mPics;

    public PicPageAdapter(@NonNull FragmentActivity fragmentActivity, List<String> pics) {
        super(fragmentActivity);
        this.mPics = pics;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        return BigPicDetailFragment.newInstance(mPics.get(position));
    }

    @Override
    public int getItemCount() {
        return mPics.size();
    }
}
