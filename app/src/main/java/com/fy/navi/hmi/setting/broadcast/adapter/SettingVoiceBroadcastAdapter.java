package com.fy.navi.hmi.setting.broadcast.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemRecommenedVoiceBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SettingVoiceBroadcastAdapter extends RecyclerView.Adapter<SettingVoiceBroadcastAdapter.Holder>{

    private static final String TAG = "SettingVoiceBroadcastAdapter";

    private List<VoiceInfo> mVoiceInfoList;
    private OnItemClickListener mOnItemClickListener;

    /**
     * 列表刷新数据
     * @param voiceInfoList
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setData(final HashMap<Integer, VoiceInfo> voiceInfoList) {
        mVoiceInfoList = new ArrayList<>(voiceInfoList.values());
        notifyDataSetChanged();
    }

    /**
     * 单选设置
     * @param index
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setSingleChoice(final int index) {
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            mVoiceInfoList.get(i).setUsed(i == index);
        }
        notifyDataSetChanged();
    }

    /**
     * 设置数据列表为Fale
     */
    @SuppressLint("NotifyDataSetChanged")
    public void unSelectAllVoices() {
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            mVoiceInfoList.get(i).setUsed(false);
        }
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener onItemClickListener) {
        this.mOnItemClickListener = onItemClickListener;
    }

    @NonNull
    @Override
    public SettingVoiceBroadcastAdapter.Holder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final ItemRecommenedVoiceBinding itemRecommenedVoiceBinding = DataBindingUtil.inflate(
                LayoutInflater.from(parent.getContext()), R.layout.item_recommened_voice, parent, false);
        return new Holder(itemRecommenedVoiceBinding);
    }

    @Override
    public void onBindViewHolder(final @NonNull SettingVoiceBroadcastAdapter.Holder holder, final int position) {
        holder.mVoiceBroadcastBinding.recommendVoiceProgress.setProgressDrawable(
                AppContext.getInstance().getMApplication().getDrawable(R.drawable.progress_bar_style));
        holder.mVoiceBroadcastBinding.setModel(mVoiceInfoList.get(position));

        holder.mVoiceBroadcastBinding.recommendVoiceOperate.setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onOperation(position);
            }
        });
    }

    @Override
    public int getItemCount() {
        if(mVoiceInfoList == null){
            return 0;
        }
        return mVoiceInfoList.size();
    }

    public static class Holder extends RecyclerView.ViewHolder{
        public ItemRecommenedVoiceBinding mVoiceBroadcastBinding;
        public Holder(final ItemRecommenedVoiceBinding voiceBroadcastBinding) {
            super(voiceBroadcastBinding.getRoot());
            this.mVoiceBroadcastBinding = voiceBroadcastBinding;
            voiceBroadcastBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener{
        /**
         * 操作
         * @param index
         */
        void onOperation(int index);
    }
}
