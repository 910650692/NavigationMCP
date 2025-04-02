package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSecondaryPoiItemBinding;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.ui.view.SkinConstraintLayout;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

public class RouteSecondaryPoiAdapter extends RecyclerView.Adapter<RouteSecondaryPoiAdapter.Holder>{
    private List<ChildInfo> mChildInfoList;
    private OnItemClickListener mItemClickListener;
    private int mSelected = -1;

    public RouteSecondaryPoiAdapter() {
        mChildInfoList = new ArrayList<>();
    }

    /***
     * 设置子节点
     * @param childInfoList 子节点列表
     */
    public void setChildInfoList(final List<ChildInfo> childInfoList) {
        if (null == childInfoList) {
            return;
        }
        mChildInfoList.clear();
        mChildInfoList.addAll(childInfoList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteSecondaryPoiItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_secondary_poi_item, parent, false);
        return new RouteSecondaryPoiAdapter.Holder(routeItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        if (mSelected == position) {
            holder.mImageView.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_secondary_poi_selected));
        } else {
            holder.mImageView.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_secondary_poi_unselected));
        }
        holder.mTextView.setText(mChildInfoList.get(position).getName());
        holder.mImageView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                final int lastSelect = mSelected;
                mSelected = position;
                notifyItemChanged(lastSelect);
                notifyItemChanged(mSelected);
                if (mItemClickListener == null) {
                    return;
                }
                mItemClickListener.onItemClick(mChildInfoList.get(position));
            }
        });
        holder.mLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                return;
            }
        });
    }

    @Override
    public int getItemCount() {
        return mChildInfoList.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSecondaryPoiItemBinding mRouteSecondaryPoiItemBinding;
        private SkinTextView mTextView;
        private SkinImageView mImageView;
        private SkinConstraintLayout mLayout;

        public Holder(@NonNull final RouteSecondaryPoiItemBinding routeSecondaryPoiItemBinding) {
            super(routeSecondaryPoiItemBinding.getRoot());
            this.mRouteSecondaryPoiItemBinding = routeSecondaryPoiItemBinding;
            mRouteSecondaryPoiItemBinding.setHolder(this);
            mTextView = mRouteSecondaryPoiItemBinding.tvRouteSecondaryPoi;
            mImageView = mRouteSecondaryPoiItemBinding.ivRouteSecondaryPoi;
            mLayout = mRouteSecondaryPoiItemBinding.lyRouteSecondaryPoi;
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param childInfo 子节点对象
         */
        void onItemClick(ChildInfo childInfo);
    }
}
