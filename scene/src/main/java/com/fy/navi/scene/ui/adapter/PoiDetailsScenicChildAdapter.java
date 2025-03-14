package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RoutePoiGasStationItemBinding;
import com.fy.navi.scene.databinding.RoutePoiIconItemBinding;
import com.fy.navi.scene.databinding.ScenePoiDetailsScenicChildSpotBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.ChildInfo;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

public class PoiDetailsScenicChildAdapter extends RecyclerView.Adapter<PoiDetailsScenicChildAdapter.Holder> {
    private List<ChildInfo> mChildList;
    OnItemClickListener itemClickListener;
    private boolean isCollapse = true;

    public boolean isCollapse() {
        return isCollapse;
    }

    public void setCollapse(boolean collapse) {
        isCollapse = collapse;
    }

    public PoiDetailsScenicChildAdapter() {
        mChildList = new ArrayList<>();
    }


    public void setChildInfoList(List<ChildInfo> childInfoList) {
        if (null == mChildList) {
            return;
        }

        mChildList.clear();
        mChildList.addAll(childInfoList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ScenePoiDetailsScenicChildSpotBinding scenePoiDetailsScenicChildSpotBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.scene_poi_details_scenic_child_spot, parent, false);
        return new Holder(scenePoiDetailsScenicChildSpotBinding);
    }

    @Override
    public int getItemCount() {
        if (mChildList == null) {
            return 0;
        }
        if (isCollapse) {
            return Math.min(mChildList.size(), 2);
        }
        return mChildList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        ChildInfo childInfo = mChildList.get(position);
        int ratio = (int) Math.round(childInfo.getRatio());
        String subTitle = holder.scenePoiDetailsScenicChildSpotBinding.childTitle.getContext().
                getString(R.string.scenic_ratio, ratio);
        Logger.d(SEARCH_HMI_TAG, "check = " + childInfo.getChecked() + " name: " + childInfo.getShortName());
        holder.scenePoiDetailsScenicChildSpotBinding.childTitle.setText(childInfo.getShortName());
        holder.scenePoiDetailsScenicChildSpotBinding.childSubTitle.setText(subTitle);
        holder.scenePoiDetailsScenicChildSpotBinding.childTitle.setSelected(childInfo.getChecked() == 1);
        holder.scenePoiDetailsScenicChildSpotBinding.childSubTitle.setSelected(childInfo.getChecked() == 1);
        holder.scenePoiDetailsScenicChildSpotBinding.poiChildLayout.setSelected(childInfo.getChecked() == 1);
        holder.scenePoiDetailsScenicChildSpotBinding.getRoot().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (itemClickListener != null) {
                    for (int i = 0; i < mChildList.size(); i++) {
                        if (i == position) {
                            mChildList.get(i).setChecked(1);
                        } else {
                            mChildList.get(i).setChecked(-1);
                        }
                    }
                    itemClickListener.onItemClick(position, true);
                }
                notifyDataSetChanged();
            }
        });
    }

    public class Holder extends RecyclerView.ViewHolder {
        public ScenePoiDetailsScenicChildSpotBinding scenePoiDetailsScenicChildSpotBinding;

        public Holder(ScenePoiDetailsScenicChildSpotBinding scenePoiDetailsScenicChildSpotBinding) {
            super(scenePoiDetailsScenicChildSpotBinding.getRoot());
            this.scenePoiDetailsScenicChildSpotBinding = scenePoiDetailsScenicChildSpotBinding;
            scenePoiDetailsScenicChildSpotBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int index, boolean isSelectIndex);
    }
}